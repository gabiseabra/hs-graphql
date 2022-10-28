{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GraphQL.Parser
  ( module GraphQL.Parser.Types
  , module GraphQL.Parser.Validation
  , getExecutableOperation
  , parseDocument
  ) where

import           Control.Arrow                  ( (&&&)
                                                , (>>>)
                                                )
import qualified Control.Comonad.Trans.Cofree  as CofreeT
import           Control.Lens                   ( view )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.Error.Class      ( MonadError(..) )
import qualified Data.Aeson                    as JSON
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Bitraversable             ( bitraverse )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Functor.Const             ( Const(..) )
import           Data.Functor.Foldable          ( Base
                                                , Recursive(..)
                                                )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Functor.Sum               ( Sum(..) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Monoid                    ( All(..) )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GraphQL.Internal               ( cataM
                                                , hoistCofreeM
                                                )
import           GraphQL.Parser.Lexer           ( mkPos )
import           GraphQL.Parser.Parser
import           GraphQL.Parser.Types
import           GraphQL.Parser.Validation
import qualified GraphQL.Response              as E
import           System.IO                      ( FilePath )
import           Text.Megaparsec                ( SourcePos
                                                , parse
                                                , unPos
                                                )
import           Text.Megaparsec.Error          ( ErrorFancy(..)
                                                , ErrorItem(..)
                                                , ParseError(..)
                                                , ParseErrorBundle(..)
                                                , attachSourcePos
                                                , errorOffset
                                                )

-- Extracts one operation from a document and returns an executable operation
-- while performing the following validations:
-- - Requested operation name is valid
-- - No required variables are missing
getExecutableOperation
  :: MonadError (NonEmpty E.GraphQLError) m
  => JSON.Object
  -> Maybe Name
  -> Document (Tree (Field Value))
  -> m (Operation (Tree (Field JSON.Value)))
getExecutableOperation input opName doc = do
  op   <- getOperation opName doc
  vars <-
    HashMap.traverseWithKey (resolveVariable input) . view _opVariables $ op
  traverse (hoistCofreeM $ bitraverse (traverse $ resolveValue vars) pure) op

getOperation
  :: MonadError (NonEmpty E.GraphQLError) m
  => Maybe Name
  -> Document a
  -> m (Operation a)
getOperation Nothing (Document _ (InL (Identity op))) = pure op
getOperation Nothing (Document _ (InR _)) = throwError . pure $ E.GraphQLError
  E.VALIDATION_ERROR
  Nothing
  Nothing
  "Operation name is required on documents with multiple operations"
getOperation (Just name) (Document _ (InL (Identity op)))
  | opName op == Just name
  = pure op
  | otherwise
  = throwError
    .  pure
    $  E.GraphQLError E.VALIDATION_ERROR Nothing Nothing
    $  "Operation "
    <> name
    <> " is not defined"
getOperation (Just name) (Document _ (InR ops)) =
  case HashMap.lookup name ops of
    Nothing ->
      throwError
        .  pure
        $  E.GraphQLError E.VALIDATION_ERROR Nothing Nothing
        $  "Operation "
        <> name
        <> " is not defined"
    Just op -> pure op

resolveValue
  :: forall m
   . (Applicative m, MonadError (NonEmpty E.GraphQLError) m)
  => HashMap Name (Variable, JSON.Value)
  -> Value
  -> m JSON.Value
resolveValue vars = cataM alg
 where
  alg :: Base Value JSON.Value -> m JSON.Value
  alg (pos CofreeT.:< InR val      ) = pure (JSON.toJSON val)
  alg (pos CofreeT.:< InL (Const k)) = case HashMap.lookup k vars of
    Nothing ->
      throwError
        .  pure
        $  E.GraphQLError E.VALIDATION_ERROR (Just [pos]) Nothing
        $  "Variable $"
        <> k
        <> " is not defined"
    Just (_, val) -> pure val

resolveVariable
  :: MonadError (NonEmpty E.GraphQLError) m
  => JSON.Object
  -> Name
  -> Variable
  -> m (Variable, JSON.Value)
resolveVariable = HashMap.filter (/= JSON.Null) >>> \input k var ->
  case HashMap.lookup k input of
    Just val -> pure (var, val)
    Nothing
      | Just val <- varDefaultValue var
      -> pure (var, cata (JSON.toJSON . CofreeT.tailF) val)
      | isNullable (varTypeDefinition var)
      -> pure (var, JSON.Null)
      | otherwise
      -> throwError
        .  pure
        $  E.GraphQLError E.VALIDATION_ERROR (Just [varPos var]) Nothing
        $  "Required variable $"
        <> k
        <> " is missing from input"

-- | Parses a raw GraphQL document from a file
parseDocument
  :: MonadError (NonEmpty E.GraphQLError) m
  => FilePath
  -> Text
  -> m (Document (Selection (Field Value)))
parseDocument file name = case parse documentP file name of
  Right doc -> pure doc
  Left e ->
    throwError . ne . foldMap (formatParseError . second mkPos) $ errorsWithPos
      e
 where
  errorsWithPos e =
    fst $ attachSourcePos errorOffset (bundleErrors e) (bundlePosState e)
  ne [] =
    E.GraphQLError E.SYNTAX_ERROR Nothing Nothing "Unknown syntax error" :| []
  ne (x : xs) = x :| xs

formatParseError :: (ParseError Text E.GraphQLError, E.Pos) -> [E.GraphQLError]
formatParseError (FancyError _ e, pos) =
  formatFancyError . (, pos) <$> Set.elems e
formatParseError (TrivialError _ actual ref, pos) =
  pure $ E.GraphQLError E.SYNTAX_ERROR (Just [pos]) Nothing $ Text.intercalate
    " "
    [ maybe "" unexpected actual
    , "Expected: "
    , Text.intercalate " | " $ showErrorItem <$> Set.toList ref
    ]
  where unexpected a = "Unexpected " <> showErrorItem a <> "."

formatFancyError :: (ErrorFancy E.GraphQLError, E.Pos) -> E.GraphQLError
formatFancyError (ErrorCustom e, _) = e
formatFancyError (ErrorIndentation ord ref actual, pos) =
  E.GraphQLError E.SYNTAX_ERROR (Just [pos]) Nothing $ Text.intercalate
    " "
    [ "Incorrect indentation: got"
    , Text.pack (show $ unPos actual) <> ","
    , "should be"
    , showOrd ord
    , Text.pack (show $ unPos ref)
    ]
formatFancyError (ErrorFail msg, pos) =
  E.GraphQLError E.SYNTAX_ERROR (Just [pos]) Nothing $ Text.pack msg

showErrorItem :: ErrorItem Char -> Text
showErrorItem (Tokens ts ) = Text.pack $ NE.toList ts
showErrorItem (Label  lbl) = Text.pack $ NE.toList lbl
showErrorItem EndOfInput   = "end of input"

showOrd GT = "more than"
showOrd LT = "less than"
showOrd EQ = "equal to"
