{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , RankNTypes
  , RecordWildCards
  , TypeOperators
#-}

module GraphQL.AST.Validation
  ( collectFields
  , getExecutableOperation
  , validateDocumentP
  , validateVarP
  , validationErrorP
  ) where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser)
import GraphQL.Response (V, Pos(..))
import qualified GraphQL.Response as E
import GraphQL.TypeSystem.Main (OperationType(..))

import GHC.Generics ((:+:)(..))

import Control.Applicative (liftA2)
import Control.Comonad.Cofree (Cofree(..), ComonadCofree(..), unfoldM)
import Control.Monad (join, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.State.Lazy (StateT(..))
import Control.Arrow ((>>>), (&&&))
import qualified Control.Monad.State.Lazy as ST
import qualified Data.Aeson as JSON
import Data.Function (on)
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function (fix)
import Data.Functor.Base (TreeF(..))
import Text.Megaparsec (customFailure)
import Data.Monoid (All(..))
import Data.Functor.Foldable (Recursive(..), Base)
import Data.Functor.Identity (Identity(..))
import Lens.Micro (traverseOf)

collectFields :: Document (Selection a) -> V (Document (Tree a))
collectFields doc = do
  (doc', visited) <- runStateT (collectFieldsST doc) mempty
  let unused = HashMap.filterWithKey (\k _ -> Set.notMember k visited) $ fragments doc'
  if length visited == length (fragments doc)
    then pure doc'
    else E.validationError (fmap fragPos . HashMap.elems $ unused)
          $ "Document has unused fragments: "
          <> Text.intercalate ", " (HashMap.keys unused)

collectFieldsST :: Document (Selection a) -> StateT (Set Name) V (Document (Tree a))
collectFieldsST doc = do
  frags <- traverse (traverseFrag $ fragments doc) $ fragments doc
  ops   <- traverse (traverseOp frags) $ operations doc
  pure $ Document frags ops
  where
    traverseFrag frags = traverseOf _fragSelection $ traverseAccum (eraseFragments $ visitFrag frags)
    traverseOp   frags = traverseOf _opSelection $ traverseAccum (eraseFragments $ pickFrag frags)

eraseFragments
  :: Monad f
  => (Pos -> Name -> f (NonEmpty (Tree a)))
  -> Selection a
  -> f (NonEmpty (Tree a))
eraseFragments f (pos :< Node a as          ) = pure . (pos :<) . NodeF a <$> traverseAccum (fmap NE.toList . eraseFragments f) as
eraseFragments f (_   :< InlineFragment _ as) = traverseAccum (eraseFragments f) as
eraseFragments f (pos :< FragmentSpread k   ) = f pos k

pickFrag :: HashMap Name (Fragment (Tree a)) -> Pos -> Name -> StateT (Set Name) V (NonEmpty (Tree a))
pickFrag frags pos k = do
  ST.modify (Set.insert k)
  case HashMap.lookup k frags of
    Just frag -> pure $ fragSelection frag
    Nothing -> lift $ E.validationError [pos] $ "Fragment " <> k <> " is not defined"

visitFrag :: HashMap Name (Fragment (Selection a)) -> Pos -> Name -> StateT (Set Name) V (NonEmpty (Tree a))
visitFrag frags = fix $ \f pos k -> do
  visited <- ST.get
  if Set.member k visited
    then lift $ E.validationError [pos] $ "Cycle in fragment " <> k
    else case HashMap.lookup k frags of
      Just frag -> do
        ST.put (Set.insert k visited)
        traverseAccum (eraseFragments f) $ fragSelection frag
      Nothing   -> lift $ E.validationError [pos] $ "Fragment " <> k <> " is not defined"

getExecutableOperation :: JSON.Object -> Maybe Name -> Document (Tree (Field Name)) -> V ExecutableOperation
getExecutableOperation input opName = mkExecutable input <=< getOperation opName

getOperation :: Maybe Name -> Document a -> V (Operation a)
getOperation Nothing     (Document _ (L1 (Identity op))) = pure op
getOperation Nothing     (Document _ (R1 _            )) = E.validationError [] "Operation name is required for documents with multiple operations"
getOperation (Just name) (Document _ (L1 (Identity op)))
  | opName op == (Just name) = pure op
  | otherwise = E.validationError [] $ "Operation " <> name <> " is not defined"
getOperation (Just name) (Document _ (R1 ops)) = case HashMap.lookup name ops of
  Nothing -> E.validationError [] $ "Operation " <> name <> " is not defined"
  Just op -> pure op

mkExecutable :: JSON.Object -> Operation (Tree (Field Name)) -> V ExecutableOperation
mkExecutable input op = do
  vars <- resolveVariables input $ opVariables op
  traverse (hoistCofreeM $ bitraverse (traverse $ resolveValue vars) pure) op

resolveValue :: Variables a -> Value Name -> V (Value (Name, Variable a))
resolveValue vars (pos:<Val v) = (pos:<) . Val <$> traverse (resolveValue vars) v
resolveValue vars (pos:<Var k) = case HashMap.lookup k vars of
  Nothing  -> E.validationError [pos] $ "Variable $" <> k <> " is not defined"
  Just var -> pure $ (pos:<Var (k, var))

-- | Resolve document variables from an input object
resolveVariables :: JSON.Object -> Variables (Maybe JSON.Value) -> V (Variables JSON.Value)
resolveVariables = HashMap.filter (/= JSON.Null) >>> \input -> HashMap.traverseWithKey $ \k var ->
  case HashMap.lookup k input of
    Just val -> pure $ var { varValue = val }
    Nothing | Just val <- varValue var -> pure $ var { varValue = val }
            | isNullable (varDefinition var) -> pure $ var { varValue = JSON.Null }
            | otherwise -> E.validationError [varPos var] $ "Required variable $" <> k <> " is missing from input"

validateDocumentP :: ([Fragment a], [Operation a]) -> Parser (Document a)
validateDocumentP (frags, ops) = Document <$> validateFragmentsP frags <*> validateOperationsP ops

-- Validates that each document either has only one operation or all named operations
validateOperationsP :: [Operation a] -> Parser ((Identity :+: HashMap Name) (Operation a))
validateOperationsP [] = parseErrorP [] "Expected at least one root operation, found none"
validateOperationsP (op:[]) = pure $ L1 $ pure op
validateOperationsP ops = R1 <$> (sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) =<< mapM go ops)
  where
    go op | Just name <- opName op = pure (name, pure op)
          | otherwise = validationErrorP [opPos op] $ "Unnamed operation in document with multiple operations"
    onDupe k op0 op1 = validationErrorP [opPos op0, opPos op1] $ "Duplicated operation name " <> k

-- Validates that fragment names are unique
validateFragmentsP :: [Fragment a] -> Parser (HashMap Name (Fragment a))
validateFragmentsP = sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) . fmap (fragName &&& pure)
  where
    onDupe k f0 f1 = validationErrorP [fragPos f0, fragPos f1] $ "Duplicated fragment name " <> k

-- Validates that a default value in a variable matches its type definitions
validateVarP :: Variable (Maybe ConstValue) -> Parser (Variable (Maybe JSON.Value))
validateVarP var@(Variable _ _   Nothing          ) = pure $ var { varValue = Nothing }
validateVarP var@(Variable _ def (Just (pos:<val))) =
  if typeMatches def val
    then pure $ var { varValue = Just $ constValToJSON val }
    else parseErrorP [pos]
          $ "Expected "
          <> Text.pack (show def)
          <> ", found "
          <> Text.pack (showConstVal val)

parseErrorP :: [Pos] -> Text -> Parser a
parseErrorP pos msg = customFailure $ E.ParseError pos msg

validationErrorP :: [Pos] -> Text -> Parser a
validationErrorP pos msg = customFailure $ E.ValidationError pos msg

traverseAccum :: (Monad m, Traversable f, Monad f) => (a -> m (f b)) -> f a -> m (f b)
traverseAccum f = pure . join <=< traverse f

hoistCofreeM :: (Traversable f, Monad m) => (forall x . f x -> m (g x)) -> Cofree f a -> m (Cofree g a)
hoistCofreeM f (x:<y) = (x:<) <$> (f =<< traverse (hoistCofreeM f) y)

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb = join (liftA2 f ma mb)

isNullable :: TypeDefinition -> Bool
isNullable (NonNullType _) = False
isNullable _               = True

typeMatches :: TypeDefinition -> ConstValueF ConstValue -> Bool
typeMatches (ListType    t) (ListVal as) = getAll $ foldMap (All . typeMatches t . unwrap) as
typeMatches (ListType    t) _            = False
typeMatches (NonNullType t) NullVal      = False
typeMatches (NonNullType t) v            = typeMatches t v
typeMatches _               v            = True
