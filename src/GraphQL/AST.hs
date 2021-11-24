{-# LANGUAGE
    TupleSections
  , GADTs
  , OverloadedStrings
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
import Data.Functor.Foldable.Monadic (cataM)
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

-- Extracts one operation from a document and returns an executable operation
-- while performing the following validations:
-- - Requested operation name is valid
-- - No required variables are missing
getExecutableOperation :: JSON.Object -> Maybe Name -> Document (Tree (Field Value)) -> V (Operation (Tree (Field JSON.Value)))
getExecutableOperation input opName doc = do
  op <- getOperation opName doc
  vars <- HashMap.traverseWithKey (resolveVariable input) . view _opVariables $ op
  traverse (hoistCofreeM $ bitraverse (traverse $ resolveValue vars) pure) op

getOperation :: Maybe Name -> Document a -> V (Operation a)
getOperation Nothing     (Document _ (InL (Identity op))) = pure op
getOperation Nothing     (Document _ (InR _            )) = E.validationError [] "Operation name is required for documents with multiple operations"
getOperation (Just name) (Document _ (InL (Identity op)))
  | opName op == Just name = pure op
  | otherwise = E.validationError [] $ "Operation " <> name <> " is not defined"
getOperation (Just name) (Document _ (InR ops)) = case HashMap.lookup name ops of
  Nothing -> E.validationError [] $ "Operation " <> name <> " is not defined"
  Just op -> pure op

resolveValue :: HashMap Name (Variable, JSON.Value) -> Value -> V JSON.Value
resolveValue vars = cataM alg
  where
    alg :: Base Value JSON.Value -> V JSON.Value
    alg (pos CofreeT.:< InR val) = pure (JSON.toJSON val)
    alg (pos CofreeT.:< InL (Const k)) = case HashMap.lookup k vars of
      Nothing -> E.validationError [pos] $ "Variable $" <> k <> " is not defined"
      Just (_,val) -> pure val

resolveVariable :: JSON.Object -> Name -> Variable -> V (Variable, JSON.Value)
resolveVariable = HashMap.filter (/= JSON.Null) >>> \input k var ->
  case HashMap.lookup k input of
    Just val -> pure (var, val)
    Nothing | Just val <- varValue var -> pure (var,cata (JSON.toJSON . CofreeT.tailF) val)
            | isNullable (varDefinition var) -> pure (var,JSON.Null)
            | otherwise -> E.validationError [varPos var] $ "Required variable $" <> k <> " is missing from input"

-- | Parses a raw GraphQL document from a file
parseDocument :: FilePath -> Text -> V (Document (Selection (Field Value)))
parseDocument file = first err . parse documentP file

err :: ParseErrorBundle Text GraphQLError -> NonEmpty GraphQLError
err e = ne $ foldMap (formatParseError . second mkPos) errorsWithPos
  where
    errorsWithPos = fst $ attachSourcePos errorOffset (bundleErrors e) (bundlePosState e)
    ne [] = ParseError [] "Unknown parse error" :| []
    ne (a:as) = a :| as

formatParseError :: (ParseError Text GraphQLError, Pos) -> [GraphQLError]
formatParseError (FancyError _ e, pos) = formatFancyError . (,pos) <$> Set.elems e
formatParseError (TrivialError _ actual ref, pos)
  = pure
  $ ParseError [pos]
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
  = ParseError [pos]
  $ Text.intercalate " "
    [ "Incorrect indentation: got"
    ,  Text.pack (show $ unPos actual) <> ","
    , "should be"
    , showOrd ord
    , Text.pack (show $ unPos ref)
    ]
formatFancyError (ErrorFail msg, pos)
  = ParseError [pos]
  $ Text.pack msg

showErrorItem :: ErrorItem Char -> Text
showErrorItem (Tokens ts) = Text.pack $ NE.toList ts
showErrorItem (Label lbl) = Text.pack $ NE.toList lbl
showErrorItem EndOfInput  = "end of input"

showOrd GT = "more than"
showOrd LT = "less than"
showOrd EQ = "equal to"
