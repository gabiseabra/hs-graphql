{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , TupleSections
#-}

module GraphQL.AST.Parser where

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
  , Var       <$> L.varName
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
      (pos, ty) <- L.withPos $ parseTypeDef
      def <- optional (L.symbol "=" *> parseVal)
      pure (ty, def, pos)

parseInput :: Parser (HashMap Name Value'RAW)
parseInput = label "Input" $ L.lexeme $ L.args L.parens L.name parseVal

parseAlias :: Parser (Maybe Name)
parseAlias = optional (try (L.name <* L.symbol ":"))

parseField :: Maybe Typename -> Parser (Pos, Field'RAW)
parseField ty = label "Field" $ L.lexeme $ do
  (pos, (alias, name)) <- L.withPos $ (,) <$> parseAlias <*> L.name
  input <- L.optional_ parseInput
  pure (pos, (ty, alias, name, input))

parseSelectionNode :: Maybe Typename -> Parser SelectionNode'RAW
parseSelectionNode ty = label "SelectionNode" $ L.lexeme $ choice
    [ inlineFragment
    , fragmentSpread
    , node
    ]
  where
    node = do
      (pos, field) <- parseField ty
      selection    <- L.optional_ $ parseSelectionSet Nothing
      pure (pos :< Node field selection)
    inlineFragment = do
      (pos, ty) <- try $ L.symbol "..." *> L.symbol "on" *> L.withPos L.name
      selection <- parseSelectionSet $ Just ty
      pure (pos :< InlineFragment ty selection)
    fragmentSpread = do
      (pos, name) <- L.symbol "..." *> L.withPos L.name
      pure (pos :< FragmentSpread name)

parseSelectionSet :: Maybe Typename -> Parser [SelectionNode'RAW]
parseSelectionSet ty = label "SelectionSet" $ L.lexeme $ L.braces $ some $ parseSelectionNode ty

parseFragment :: Parser (Text, Fragment'RAW)
parseFragment = label "Fragment" $ L.lexeme $ do
  (pos, name) <- L.symbol "fragment" *> L.withPos L.name
  ty          <- L.symbol "on" *> L.name
  selection   <- parseSelectionSet $ Just ty
  pure (name, (ty, selection, pos))

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
