{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , TupleSections
#-}

module GraphQL.AST.Parser (parseRootNodes) where

import GraphQL.AST.Document
import GraphQL.AST.Validation
import GraphQL.AST.Lexer (Parser, (<@>))
import qualified GraphQL.AST.Lexer as L

import Control.Arrow ((+++))
import Control.Comonad.Cofree (Cofree(..))
import Control.Applicative ((<|>))
import Text.Megaparsec (label, try, choice, optional, many, some, eitherP, eof, getSourcePos, customFailure)
import Text.Megaparsec.Char (string, char)
import Data.Bitraversable (bisequence)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text

parseOperationType :: Parser OperationType
parseOperationType = label "OperationType" $ L.lexeme $ choice
  [ Query        <$ string "query"
  , Mutation     <$ string "mutation"
  , Subscription <$ string "subscription"
  ]

parseTypeDef :: Parser TypeDefinition
parseTypeDef = label "VariableDefinition" $ L.lexeme $ do
  var <- choice
          [ ListType  <$> L.brackets parseTypeDef
          , NamedType <$> L.name
          ]
  required <- isJust <$> optional (char '!')
  if required
    then pure $ NonNullType var
    else pure var

parseVal :: Parser Value'RAW
parseVal = label "Value" $ choice
  [ NullVal   <$  L.symbol "null"
  , VarVal    <$> L.varName
  , StrVal    <$> L.stringVal
  , IntVal    <$> try L.intVal
  , DoubleVal <$> L.doubleVal
  , BoolVal   <$> L.boolVal
  , EnumVal   <$> L.enumVal
  , ListVal   <$> L.brackets (many parseVal)
  , ObjectVal <$> L.args L.braces L.name parseVal
  ] <@> (:<)

parseVars :: Parser (HashMap Name Variable'RAW)
parseVars = label "Variables" $ L.lexeme $ L.argsE L.parens L.varName pV validateVarP
  where
    pV = do
      (loc, ty) <- L.withLocation $ parseTypeDef
      def <- optional (L.symbol "=" *> parseVal)
      pure (ty, def, loc)

parseInput :: Parser (HashMap Name Value'RAW)
parseInput = label "Input" $ L.lexeme $ L.args L.parens L.name parseVal

parseAlias :: Parser (Maybe Name)
parseAlias = optional (try (L.name <* L.symbol ":"))

parseField :: Maybe Typename -> Parser (Location, Field'RAW)
parseField ty = label "Field" $ L.lexeme $ do
  (loc, (alias, name)) <- L.withLocation $ (,) <$> parseAlias <*> L.name
  input <- parseInput
  pure (loc, (ty, alias, name, input))

parseSelectionNode :: Maybe Typename -> Parser SelectionNode'RAW
parseSelectionNode ty = label "SelectionNode" $ L.lexeme $ choice
    [ inlineFragment
    , fragmentSpread
    , node
    ]
  where
    node = do
      (loc, field) <- parseField ty
      selection    <- L.optional_ $ parseSelectionSet Nothing
      pure (loc :< Node field selection)
    inlineFragment = do
      (loc, ty) <- L.symbol "..." *> L.symbol "on" *> L.withLocation L.name
      selection <- parseSelectionSet $ Just ty
      pure (loc :< InlineFragment ty selection)
    fragmentSpread = do
      (loc, name) <- L.symbol "..." *> L.withLocation L.name
      pure (loc :< FragmentSpread name)

parseSelectionSet :: Maybe Typename -> Parser [SelectionNode'RAW]
parseSelectionSet ty = label "SelectionSet" $ L.lexeme $ L.braces $ some $ parseSelectionNode ty

parseFragment :: Parser (Text, Fragment'RAW)
parseFragment = label "Fragment" $ L.lexeme $ do
  (loc, name) <- L.symbol "fragment" *> L.withLocation L.name
  ty          <- L.symbol "on" *> L.name
  selection   <- parseSelectionSet $ Just ty
  pure (name, (ty, selection, loc))

parseOperation :: Parser Operation'RAW
parseOperation = label "Operation" $ L.lexeme $
  ( (,,,) <$> parseOperationType
          <*> try (optional L.name)
          <*> try (L.optional_ parseVars)
          <*> parseSelectionSet Nothing
  ) <@> extend5

parseRootNodes :: Parser RootNodes'RAW
parseRootNodes = label "Document" $ L.sc *> nodes <* L.sc <* eof
  where
    nodes = validateRootNodesP =<< L.foldE p mempty
    p ab  = appendEither ab <$> eitherP parseOperation parseFragment

extend5 e (a,b,c,d) = (a,b,c,d,e)

appendEither :: ([a], [b]) -> Either a b -> ([a], [b])
appendEither (as, bs) (Right b) = (as, b:bs)
appendEither (as, bs) (Left a) = (a:as, bs)
