{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , TupleSections
  , TypeOperators
#-}

module GraphQL.AST.Parser where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser, (<@>))
import qualified GraphQL.AST.Lexer as L
import GraphQL.Response (Pos)
import qualified GraphQL.Response as E
import GraphQL.TypeSystem.Main (OperationType(..))

import Control.Arrow ((+++), (&&&))
import Control.Applicative ((<|>), liftA2)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad (join)
import Control.Monad.Trans (lift)
import Control.Monad.Combinators.NonEmpty (some)
import qualified Data.Aeson as JSON
import Data.Bitraversable (bisequence)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (partitionEithers)
import qualified Data.Vector as Vec
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Fix (Fix(..))
import Data.Function (fix)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Sum (Sum(..))
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

operationTypeP :: Parser OperationType
operationTypeP = label "OperationType" $ option QUERY $ try $ choice
  [ QUERY        <$ L.symbol "query"
  , MUTATION     <$ L.symbol "mutation"
  , SUBSCRIPTION <$ L.symbol "subscription"
  ]

typeDefP :: Parser TypeDefinition
typeDefP = label "VariableDefinition" $ L.lexeme $ Fix <$> do
  var <- choice
          [ ListType  <$> L.brackets typeDefP
          , NamedType <$> L.name
          ]
  required <- isJust <$> optional (char '!')
  if required
    then pure . NonNullType . Fix $ var
    else pure var

defValueP :: Parser (Maybe ConstValue)
defValueP = optional (L.symbol "=" *> constValueP)

constValueFP :: Parser r -> Parser (ConstValueF r)
constValueFP p = label "ConstValue" $ choice
  [ NullVal                <$  L.symbol "null"
  , StrVal                 <$> L.stringVal
  , IntVal                 <$> try L.intVal
  , DoubleVal              <$> L.doubleVal
  , BoolVal                <$> L.boolVal
  , EnumVal                <$> L.enumVal
  , ListVal . Vec.fromList <$> L.brackets (many p)
  , ObjectVal              <$> L.args L.braces L.name p
  ]

constValueP :: Parser ConstValue
constValueP = fix (constValueFP . flip (<@>) (:<)) <@> (:<)

valueP :: Parser Value
valueP = label "Value" $ choice
  [ InL . Const <$> L.varName
  , InR         <$> constValueFP valueP
  ] <@> (:<)

varsP :: Parser (HashMap Name Variable)
varsP = label "Variables"
  $ L.args L.parens L.varName
  $ Variable <$> L.getPos
             <*> typeDefP
             <*> defValueP

argsP :: Parser (HashMap Name Value)
argsP = label "Arguments" $ L.args L.parens L.name valueP

aliasP :: Parser (Maybe Name)
aliasP = optional $ try (L.name <* L.symbol ":")

fieldP :: Maybe Name -> Parser (Field Value)
fieldP ty = label "Field"
  $ Field ty <$> aliasP
             <*> L.name
             <*> L.optional_ argsP

selectionNodeP :: Maybe Name -> Parser (Selection (Field Value))
selectionNodeP ty = label "Selection" $ choice
    [ try inlineFragment
    , fragmentSpread
    , node
    ]
  where
    node = do
      pos   <- L.getPos
      field <- fieldP ty
      sel   <- selectionSetP_ Nothing
      pure (pos:<Node field sel)
    inlineFragment = do
      () <$ L.symbol "..." <* L.symbol "on"
      pos <- L.getPos
      ty  <- L.name
      sel <- selectionSetP $ Just ty
      pure (pos:<InlineFragment ty sel)
    fragmentSpread = do
      () <$ L.symbol "..."
      pos  <- L.getPos
      name <- L.name
      pure (pos:<FragmentSpread name)

selectionP :: Maybe Name -> Parser (Selection (Field Value))
selectionP ty = label "SelectionSet" $ L.braces $ selectionNodeP ty

selectionSetP :: Maybe Name -> Parser (NonEmpty (Selection (Field Value)))
selectionSetP ty = label "SelectionSet" $ L.braces $ some $ selectionNodeP ty

selectionSetP_ :: Maybe Name -> Parser [Selection (Field Value)]
selectionSetP_ ty = maybe [] NE.toList <$> optional (selectionSetP ty)

fragmentP :: Parser (Fragment (Selection (Field Value)))
fragmentP = label "Fragment" $ do
  () <$ L.symbol "fragment"
  pos  <- L.getPos
  name <- L.name
  ty   <- L.symbol "on" *> L.name
  Fragment pos name ty <$> selectionSetP (Just ty)

operationP :: Parser (Operation (Selection (Field Value)))
operationP = label "Operation" $ do
  pos    <- L.getPos
  opType <- operationTypeP
  name   <- optional L.name
  vars   <- try (L.optional_ varsP)
  case opType of
    QUERY        -> Query pos name vars <$> selectionSetP Nothing
    MUTATION     -> Mutation pos name vars <$> selectionSetP Nothing
    SUBSCRIPTION -> Subscription pos name vars <$> selectionP Nothing

documentP :: Parser (Document (Selection (Field Value)))
documentP = label "Document" $ L.sc *> nodes <* L.sc <* eof
  where
    nodes = validateDocumentP =<< L.foldE p mempty
    p ab  = appendEither ab <$> eitherP fragmentP operationP

-- Validates a document's immediate children
validateDocumentP :: ([Fragment a], [Operation a]) -> Parser (Document a)
validateDocumentP (frags, ops) = Document <$> validateFragmentsP frags <*> validateOperationsP ops

-- Validates that each document either has only one operation or all named operations
validateOperationsP :: [Operation a] -> Parser ((Identity :+: HashMap Name) (Operation a))
validateOperationsP [] = L.syntaxErrorP [] "Expected at least one root operation, found none"
validateOperationsP [op] = pure $ InL $ pure op
validateOperationsP ops = InR <$> (sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) =<< mapM go ops)
  where
    go op | Just name <- opName op = pure (name, pure op)
          | otherwise = L.validationErrorP [opPos op] "Unnamed operation in document with multiple operations"
    onDupe k op0 op1 = L.validationErrorP [opPos op0, opPos op1] $ "Duplicated operation name " <> k

-- Validates that fragment names are unique
validateFragmentsP :: [Fragment a] -> Parser (HashMap Name (Fragment a))
validateFragmentsP = sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) . fmap (fragName &&& pure)
  where
    onDupe k frag0 frag1 = L.validationErrorP [fragPos frag0, fragPos frag1] $ "Duplicated fragment name " <> k

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb = join (liftA2 f ma mb)

appendEither :: ([a], [b]) -> Either a b -> ([a], [b])
appendEither (as, bs) (Right b) = (as, b:bs)
appendEither (as, bs) (Left a) = (a:as, bs)
