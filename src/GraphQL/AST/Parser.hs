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
import Text.Megaparsec
  ( label
  , try
  , choice
  , optional
  , option
  , many
  , some
  , eitherP
  , eof
  , customFailure
  )
import Text.Megaparsec.Char (string, char)
import Data.Bitraversable (bisequence)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text

parseOperationType :: Parser OperationType
parseOperationType = label "OperationType" $ option Query $ try $ choice
  [ Query        <$ L.symbol "query"
  , Mutation     <$ L.symbol "mutation"
  , Subscription <$ L.symbol "subscription"
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

parseDefValue :: Parser (Maybe Value'RAW)
parseDefValue = optional (L.symbol "=" *> parseVal)

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
parseVars = label "Variables" $ L.argsE validateVarP L.parens L.varName $
  Variable'RAW <$> L.getPos
               <*> parseTypeDef
               <*> parseDefValue

parseInput :: Parser (HashMap Name Value'RAW)
parseInput = label "Input" $ L.args L.parens L.name parseVal

parseAlias :: Parser (Maybe Name)
parseAlias = optional $ try (L.name <* L.symbol ":")

parseField :: Maybe Typename -> Parser Field'RAW
parseField ty = label "Field" $
  Field ty <$> parseAlias
           <*> L.name
           <*> L.optional_ parseInput

parseSelectionNode :: Maybe Typename -> Parser SelectionNode'RAW
parseSelectionNode ty = label "SelectionNode" $ choice
    [ try inlineFragment
    , fragmentSpread
    , node
    ]
  where
    node = do
      pos   <- L.getPos
      field <- parseField ty
      sel   <- L.optional_ $ parseSelectionSet Nothing
      pure (pos :< Node field sel)
    inlineFragment = do
      () <$ L.symbol "..." <* L.symbol "on"
      pos <- L.getPos
      ty  <- L.name
      sel <- parseSelectionSet $ Just ty
      pure (pos :< InlineFragment ty sel)
    fragmentSpread = do
      () <$ L.symbol "..."
      pos  <- L.getPos
      name <- L.name
      pure (pos :< FragmentSpread name)

parseSelectionSet :: Maybe Typename -> Parser [SelectionNode'RAW]
parseSelectionSet ty = label "SelectionSet" $ L.braces $ some $ parseSelectionNode ty

parseFragment :: Parser (Text, Fragment'RAW)
parseFragment = label "Fragment" $ do
  () <$ L.symbol "fragment"
  pos  <- L.getPos
  name <- L.name
  ty   <- L.symbol "on" *> L.name
  sel  <- parseSelectionSet $ Just ty
  pure $ (name, Fragment pos ty sel)

parseOperation :: Parser Operation'RAW
parseOperation = label "Operation" $
  Operation'RAW <$> L.getPos
                <*> parseOperationType
                <*> optional L.name
                <*> try (L.optional_ parseVars)
                <*> parseSelectionSet Nothing

parseRootNodes :: Parser RootNodes'RAW
parseRootNodes = label "Document" $ L.sc *> nodes <* L.sc <* eof
  where
    nodes = validateRootNodesP =<< L.foldE p mempty
    p ab  = appendEither ab <$> eitherP parseFragment parseOperation

appendEither :: ([a], [b]) -> Either a b -> ([a], [b])
appendEither (as, bs) (Right b) = (as, b:bs)
appendEither (as, bs) (Left a) = (a:as, bs)
