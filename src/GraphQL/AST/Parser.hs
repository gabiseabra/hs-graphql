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
import GraphQL.TypeSystem.Main (OperationType(..))

import Control.Arrow ((+++))
import Control.Applicative ((<|>))
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Combinators.NonEmpty (some)
import Text.Megaparsec
  ( label
  , try
  , choice
  , optional
  , option
  , many
  , eitherP
  , eof
  , customFailure
  )
import Text.Megaparsec.Char (string, char)
import qualified Data.Aeson as JSON
import Data.Bitraversable (bisequence)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (partitionEithers)
import qualified Data.Vector as Vec
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Data.Function (fix)

parseOperationType :: Parser OperationType
parseOperationType = label "OperationType" $ option QUERY $ try $ choice
  [ QUERY        <$ L.symbol "query"
  , MUTATION     <$ L.symbol "mutation"
  , SUBSCRIPTION <$ L.symbol "subscription"
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

parseDefValue :: Parser (Maybe ConstValue)
parseDefValue = optional (L.symbol "=" *> parseConstVal)

parseConstValF :: Parser r -> Parser (ConstValueF r)
parseConstValF p = label "ConstValue" $ choice
  [ NullVal                <$  L.symbol "null"
  , StrVal                 <$> L.stringVal
  , IntVal                 <$> try L.intVal
  , DoubleVal              <$> L.doubleVal
  , BoolVal                <$> L.boolVal
  , EnumVal                <$> L.enumVal
  , ListVal . Vec.fromList <$> L.brackets (many p)
  , ObjectVal              <$> L.args L.braces L.name p
  ]

parseConstVal :: Parser ConstValue
parseConstVal = fix ((<@> (:<)) . parseConstValF)

parseVal :: Parser (Value Name)
parseVal = label "Value" $ choice
  [ Var <$> L.varName
  , Val <$> parseConstValF parseVal
  ] <@> (:<)

parseVars :: Parser (HashMap Name (Variable (Maybe JSON.Value)))
parseVars = label "Variables"
  $ (>>= traverse validateVarP)
  $ L.args L.parens L.varName
  $ Variable <$> L.getPos
             <*> parseTypeDef
             <*> parseDefValue

parseArgs :: Parser (HashMap Name (Value Name))
parseArgs = label "Arguments" $ L.args L.parens L.name parseVal

parseAlias :: Parser (Maybe Name)
parseAlias = optional $ try (L.name <* L.symbol ":")

parseField :: Maybe Name -> Parser (Field Name)
parseField ty = label "Field"
  $ Field ty <$> parseAlias
             <*> L.name
             <*> L.optional_ parseArgs

parseSelectionNode :: Maybe Name -> Parser (Selection (Field Name))
parseSelectionNode ty = label "Selection" $ choice
    [ try inlineFragment
    , fragmentSpread
    , node
    ]
  where
    node = do
      pos   <- L.getPos
      field <- parseField ty
      sel   <- parseSelectionSet_ Nothing
      pure (pos:<Node field sel)
    inlineFragment = do
      () <$ L.symbol "..." <* L.symbol "on"
      pos <- L.getPos
      ty  <- L.name
      sel <- parseSelectionSet $ Just ty
      pure (pos:<InlineFragment ty sel)
    fragmentSpread = do
      () <$ L.symbol "..."
      pos  <- L.getPos
      name <- L.name
      pure (pos:<FragmentSpread name)

parseSelection :: Maybe Name -> Parser (Selection (Field Name))
parseSelection ty = label "SelectionSet" $ L.braces $ parseSelectionNode ty

parseSelectionSet :: Maybe Name -> Parser (NonEmpty (Selection (Field Name)))
parseSelectionSet ty = label "SelectionSet" $ L.braces $ some $ parseSelectionNode ty

parseSelectionSet_ :: Maybe Name -> Parser [Selection (Field Name)]
parseSelectionSet_ ty = maybe [] NE.toList <$> optional (parseSelectionSet ty)

parseFragment :: Parser (Fragment (Selection (Field Name)))
parseFragment = label "Fragment" $ do
  () <$ L.symbol "fragment"
  pos  <- L.getPos
  name <- L.name
  ty   <- L.symbol "on" *> L.name
  Fragment pos name ty <$> parseSelectionSet (Just ty)

parseOperation :: Parser (Operation (Selection (Field Name)))
parseOperation = label "Operation" $ do
  pos  <- L.getPos
  op   <- parseOperationType
  name <- optional L.name
  vars <- try (L.optional_ parseVars)
  case op of
    QUERY        -> Query pos name vars <$> parseSelectionSet Nothing
    MUTATION     -> Mutation pos name vars <$> parseSelectionSet Nothing
    SUBSCRIPTION -> Subscription pos name vars <$> parseSelection Nothing

parseRootNodes :: Parser (Document (Selection (Field Name)))
parseRootNodes = label "Document" $ L.sc *> nodes <* L.sc <* eof
  where
    nodes = validateDocumentP =<< L.foldE p mempty
    p ab  = appendEither ab <$> eitherP parseFragment parseOperation

appendEither :: ([a], [b]) -> Either a b -> ([a], [b])
appendEither (as, bs) (Right b) = (as, b:bs)
appendEither (as, bs) (Left a) = (a:as, bs)
