{-# LANGUAGE
    TupleSections
  , GADTs
  , OverloadedStrings
#-}

module GraphQL.AST
  ( Name
  , Pos(..)
  , OperationType(..)
  , TypeDefinition(..)
  , ValueF(..)
  , FieldF(..)
  , SelectionNodeF(..)
  , FragmentF(..)
  , DocumentF(..)
  , Document
  , Value
  , Fragment
  , FieldSet
  , SelectionSet
  , typeDefinition
  , parseDocument
  , collectFields
  ) where

import GraphQL.AST.Document
import GraphQL.AST.Validation
import GraphQL.AST.Parser
import GraphQL.AST.Lexer (mkPos)
import GraphQL.Response
import qualified GraphQL.TypeSystem as TS

import Control.Monad ((<=<))
import Data.Bifunctor (first, second)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
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

typeDefinition :: TS.TypeDef k a -> TypeDefinition
typeDefinition (TS.ListType     _ def _) = ListType $ typeDefinition def
typeDefinition (TS.NullableType _ def _) = case typeDefinition def of
  (NonNullType ty) -> ty
  ty               -> ty
typeDefinition def = NonNullType $ NamedType $ TS.typename def

parseDocument :: FilePath -> Maybe Name -> JSON.Object -> Text -> V (HashMap Name Fragment, DocumentF SelectionSet)
parseDocument path opName input = validateDocument opName input <=< first err . parse parseRootNodes path

err :: ParseErrorBundle Text GraphQLError -> NonEmpty GraphQLError
err e = ne $ foldMap (formatParseError . second mkPos) errorsWithPos
  where
    errorsWithPos = fst $ attachSourcePos errorOffset (bundleErrors e) (bundlePosState e)
    ne [] = ParseError [] "Unknown parse error" :| []
    ne (a:as) = a :| as

formatParseError :: (ParseError Text GraphQLError, Pos) -> [GraphQLError]
formatParseError (FancyError _ e, pos) = fmap (formatFancyError . (,pos)) $ Set.elems e
formatParseError (TrivialError _ actual ref, pos)
  = pure
  $ ParseError [pos]
  $ Text.intercalate " "
  $ [ maybe "" unexpected actual
    , "Expected: "
    , Text.intercalate " | "
    $ fmap showErrorItem
    $ Set.toList ref
    ]
  where unexpected a = "Unexpected " <> showErrorItem a <> "."

formatFancyError :: (ErrorFancy GraphQLError, Pos) -> GraphQLError
formatFancyError (ErrorCustom e, _) = e
formatFancyError (ErrorIndentation ord ref actual, pos)
  = ParseError [pos]
  $ Text.intercalate " "
  $ [ "Incorrect indentation: got"
    ,  Text.pack (show $ unPos actual) <> ","
    , "should be"
    , showOrd ord
    , Text.pack (show $ unPos ref)
    ]
formatFancyError (ErrorFail msg, pos)
  = ParseError [pos]
  $ Text.pack msg

showErrorItem :: ErrorItem Char -> Text
showErrorItem (Tokens ts)  = Text.pack $ NE.toList ts
showErrorItem (Label lbl)  = Text.pack $ NE.toList lbl
showErrorItem (EndOfInput) = "end of input"

showOrd GT = "more than"
showOrd LT = "less than"
showOrd EQ = "equal to"
