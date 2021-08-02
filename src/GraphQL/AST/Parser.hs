{-# LANGUAGE
    OverloadedStrings
  , DuplicateRecordFields
  , TupleSections
  , FlexibleContexts
#-}

module GraphQL.AST.Parser where

import GraphQL.Class (Typename)
import GraphQL.AST.Lexer (Parser)
import qualified GraphQL.AST.Lexer as L

import Control.Applicative ((<|>))
import Text.Megaparsec (label, try, choice, optional, many, some, eitherP, eof, customFailure)
import Text.Megaparsec.Char (string, char)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as Text

data OperationType
  = Query
  | Mutation
  | Subscription
  deriving (Eq, Show)

operationType :: Parser OperationType
operationType = label "OperationType" $ L.lexeme $ choice
  [ Query        <$ string "query"
  , Mutation     <$ string "mutation"
  , Subscription <$ string "subscription"
  ]

data VariableDefinition
  = ListVar VariableDefinition
  | NonNullVar VariableDefinition
  | TypeVar Typename
  deriving (Eq, Show)

variableDefinition :: Parser VariableDefinition
variableDefinition = label "VariableDefinition" $ L.lexeme $ do
  var <- choice
          [ ListVar <$> L.brackets variableDefinition
          , TypeVar <$> L.name
          ]
  required <- isJust <$> optional (char '!')
  if required
    then pure $ NonNullVar var
    else pure var

variableDefinitions :: Parser [(Text, VariableDefinition)]
variableDefinitions = label "VariableDefinitions"
  $ L.lexeme $ fmap (fromMaybe mempty) (optional $ L.vars L.parens L.varName variableDefinition)

data VariableAssignment
  = NullVal
  | Var Text
  | StrVal Text
  | IntVal Int
  | NumVal Double
  | BoolVal Bool
  | EnumVal Text
  | ListVal [VariableAssignment]
  | ObjectVal [(Text, VariableAssignment)]
  deriving (Eq, Show)

val :: Parser VariableAssignment
val = label "Value" $ choice
  [ NullVal   <$  L.symbol "null"
  , Var       <$> L.varName
  , StrVal    <$> L.stringVal
  , IntVal    <$> try L.intVal
  , NumVal    <$> L.doubleVal
  , BoolVal   <$> L.boolVal
  , EnumVal   <$> L.enumVal
  , ListVal   <$> L.brackets (many val)
  , ObjectVal <$> L.vars L.braces L.name val
  ]

variableAssignments :: Parser [(Text, VariableAssignment)]
variableAssignments = label "VariableAssignment"
  $ fmap (fromMaybe mempty) (optional $ L.vars L.parens L.name val)

data Field
  = Field
    { typename :: Maybe Typename
    , alias :: Maybe Text
    , name :: Text
    , variables :: [(Text, VariableAssignment)]
    } deriving (Eq, Show)

field :: Parser Field
field = label "Field" $ L.lexeme $
  Field Nothing
    <$> optional (try (L.name <* L.symbol ":"))
    <*> L.name
    <*> variableAssignments

setTypename :: Typename -> Field -> Field
setTypename ty f = f { typename = Just ty }

data Selection
  = Node Field [Selection]
  | FragmentSpread Text
  | InlineFragment Typename [Selection]
  deriving (Eq, Show)

selectionSet :: Maybe Typename -> Parser [Selection]
selectionSet ty = label "SelectionSet" $ L.lexeme $ L.braces $ some $ selection ty

selectionSet_ :: Maybe Typename -> Parser [Selection]
selectionSet_ ty = fmap (fromMaybe mempty) (optional $ selectionSet ty)

selection :: Maybe Typename -> Parser Selection
selection ty = label "Selection" $ L.lexeme $ choice
  [ try selection'inlineFragment
  , FragmentSpread <$> (L.symbol "..." *> L.name)
  , Node           <$> fmap (maybe id setTypename ty) field
                   <*> selectionSet_ ty
  ]

selection'inlineFragment :: Parser Selection
selection'inlineFragment = do
  ty     <- L.symbol "..." *> L.symbol "on" *> L.name
  fields <- selectionSet $ Just ty
  pure $ InlineFragment ty fields

data Fragment
  = Fragment
    { typename :: Typename
    , selectionFields :: [Selection]
    } deriving (Eq, Show)

fragment :: Parser (Text, Fragment)
fragment = label "Fragment" $ L.lexeme $ do
  name   <- L.symbol "fragment" *> L.name
  ty     <- L.symbol "on" *> L.name
  fields <- selectionSet $ Just ty
  pure (name, Fragment ty fields)

data Document
  = Document
    { operation :: OperationType
    , name :: Maybe Text
    , variables :: [(Text, VariableDefinition)]
    , selectionFields :: [Selection]
    , fragments :: [(Text, Fragment)]
    } deriving (Eq, Show)

type Operation = (OperationType, Maybe Text, [(Text, VariableDefinition)], [Selection])

type RootNode = Either Operation Fragment

rootNode = eitherP op fragment
  where
    op = label "Operation" $ L.lexeme $
      (,,,) <$> operationType
            <*> try (optional L.name)
            <*> try variableDefinitions
            <*> selectionSet Nothing

document :: Parser Document
document = label "Document" $ do
  L.sc
  ((op, name, vars, fields), frags) <- firstM validateOps . partitionEithers =<< some rootNode
  L.sc *> eof
  pure $ Document op name vars fields frags
  where
    validateOps []            = customFailure "Expected at least one operation, found none"
    validateOps ops@(_:(_:_)) = customFailure $ "Expected at most one operation, found " <> Text.pack (show $ length ops)
    validateOps (op:_)        = pure op

firstM :: Functor m => (a -> m a') -> (a, b) -> m (a', b)
firstM f (a,b) = (,b) <$> f a
