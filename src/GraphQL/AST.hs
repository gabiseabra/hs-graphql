{-# LANGUAGE
    TupleSections
  , GADTs
  , FlexibleContexts
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
#-}

module GraphQL.AST
  ( Name
  , TypeDefinitionF(..)
  , TypeDefinition
  , ConstValueF(..)
  , ConstValue
  , Value
  , Variable(..)
  , Field(..)
  , SelectionF(..)
  , Selection
  , Fragment(..)
  , Operation(..)
  , Document(..)
  , Tree
  , Att
  , ExecutableOperation
  , getExecutableOperation
  , parseDocument
  , basicRules
  ) where

import GraphQL.AST.Document
import GraphQL.AST.Validation
import GraphQL.AST.Parser
import GraphQL.AST.Lexer (mkPos)
import GraphQL.Response
import qualified GraphQL.Response as E
import qualified GraphQL.TypeSystem as TS
import GraphQL.Internal (hoistCofreeM)
import Control.Arrow ((>>>), (&&&))
import qualified Control.Comonad.Trans.Cofree as CofreeT
import Control.Monad ((<=<))
import Control.Lens (view)
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Monoid (All(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Foldable (Recursive(..), Base)
import Data.Functor.Sum (Sum(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec (SourcePos, unPos, parse)
import Text.Megaparsec.Error
  ( ParseErrorBundle(..)
  , ParseError(..)
  , ErrorFancy(..)
  , ErrorItem(..)
  , errorOffset
  , attachSourcePos
  )
import System.IO (FilePath)
import Control.Monad.Error.Class (MonadError(..))
import           GraphQL.Internal (cataM)

-- Extracts one operation from a document and returns an executable operation
-- while performing the following validations:
-- - Requested operation name is valid
-- - No required variables are missing
getExecutableOperation :: MonadError (NonEmpty GraphQLError) m => JSON.Object -> Maybe Name -> Document (Tree (Field Value)) -> m (Operation (Tree (Field JSON.Value)))
getExecutableOperation input opName doc = do
  op <- getOperation opName doc
  vars <- HashMap.traverseWithKey (resolveVariable input) . view _opVariables $ op
  traverse (hoistCofreeM $ bitraverse (traverse $ resolveValue vars) pure) op

getOperation :: MonadError (NonEmpty GraphQLError) m => Maybe Name -> Document a -> m (Operation a)
getOperation Nothing     (Document _ (InL (Identity op))) = pure op
getOperation Nothing     (Document _ (InR _            )) = graphQLError VALIDATION_ERROR [] "Operation name is required on documents with multiple operations"
getOperation (Just name) (Document _ (InL (Identity op)))
  | opName op == Just name = pure op
  | otherwise = graphQLError VALIDATION_ERROR [] $ "Operation " <> name <> " is not defined"
getOperation (Just name) (Document _ (InR ops)) = case HashMap.lookup name ops of
  Nothing -> graphQLError VALIDATION_ERROR [] $ "Operation " <> name <> " is not defined"
  Just op -> pure op

resolveValue :: forall m. (Applicative m, MonadError (NonEmpty GraphQLError) m) => HashMap Name (Variable, JSON.Value) -> Value -> m JSON.Value
resolveValue vars = cataM alg
  where
    alg :: Base Value JSON.Value -> m JSON.Value
    alg (pos CofreeT.:< InR val) = pure (JSON.toJSON val)
    alg (pos CofreeT.:< InL (Const k)) = case HashMap.lookup k vars of
      Nothing -> E.graphQLError E.VALIDATION_ERROR [pos] $ "Variable $" <> k <> " is not defined"
      Just (_,val) -> pure val

resolveVariable :: MonadError (NonEmpty GraphQLError) m => JSON.Object -> Name -> Variable -> m (Variable, JSON.Value)
resolveVariable = HashMap.filter (/= JSON.Null) >>> \input k var ->
  case HashMap.lookup k input of
    Just val -> pure (var, val)
    Nothing | Just val <- varDefaultValue var -> pure (var,cata (JSON.toJSON . CofreeT.tailF) val)
            | isNullable (varTypeDefinition var) -> pure (var,JSON.Null)
            | otherwise -> graphQLError VALIDATION_ERROR [varPos var] $ "Required variable $" <> k <> " is missing from input"

-- | Parses a raw GraphQL document from a file
parseDocument :: MonadError (NonEmpty GraphQLError) m => FilePath -> Text -> m (Document (Selection (Field Value)))
parseDocument file name = case parse documentP file name of
  Right doc -> pure doc
  Left e -> throwError . ne . foldMap (formatParseError . second mkPos) $ errorsWithPos e
  where
    errorsWithPos e = fst $ attachSourcePos errorOffset (bundleErrors e) (bundlePosState e)
    ne [] = E.GraphQLError E.SYNTAX_ERROR Nothing Nothing "Unknown syntax error":|[]
    ne (x:xs) = x:|xs

formatParseError :: (ParseError Text GraphQLError, Pos) -> [GraphQLError]
formatParseError (FancyError _ e, pos) = formatFancyError . (,pos) <$> Set.elems e
formatParseError (TrivialError _ actual ref, pos)
  = pure
  $ E.GraphQLError E.SYNTAX_ERROR (Just [pos]) Nothing
  $ Text.intercalate " "
    [ maybe "" unexpected actual
    , "Expected: "
    , Text.intercalate " | "
    $ showErrorItem <$> Set.toList ref
    ]
  where unexpected a = "Unexpected " <> showErrorItem a <> "."

formatFancyError :: (ErrorFancy GraphQLError, Pos) -> GraphQLError
formatFancyError (ErrorCustom e, _) = e
formatFancyError (ErrorIndentation ord ref actual, pos)
  = E.GraphQLError E.SYNTAX_ERROR (Just [pos]) Nothing
  $ Text.intercalate " "
    [ "Incorrect indentation: got"
    ,  Text.pack (show $ unPos actual) <> ","
    , "should be"
    , showOrd ord
    , Text.pack (show $ unPos ref)
    ]
formatFancyError (ErrorFail msg, pos)
  = E.GraphQLError E.SYNTAX_ERROR (Just [pos]) Nothing $ Text.pack msg

showErrorItem :: ErrorItem Char -> Text
showErrorItem (Tokens ts) = Text.pack $ NE.toList ts
showErrorItem (Label lbl) = Text.pack $ NE.toList lbl
showErrorItem EndOfInput  = "end of input"

showOrd GT = "more than"
showOrd LT = "less than"
showOrd EQ = "equal to"
