{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , RankNTypes
  , RecordWildCards
  , TypeOperators
  , FlexibleContexts
#-}

module GraphQL.AST.Validation
  ( collectFields
  , getExecutableOperation
  , validateDocumentP
  , validationErrorP
  ) where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser)
import GraphQL.Response (V, Pos(..))
import qualified GraphQL.Response as E
import GraphQL.TypeSystem.Main (OperationType(..))
import GraphQL.Internal (hoistCofreeM)

import GHC.Generics ((:+:)(..))

import Control.Applicative (liftA2)
import Control.Comonad.Cofree (Cofree(..), ComonadCofree(..), unfoldM)
import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Trans.Cofree as CofreeT
import Control.Monad (join, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.State.Lazy (StateT(..))
import Control.Arrow ((>>>), (&&&))
import qualified Control.Monad.State.Lazy as ST
import qualified Data.Aeson as JSON
import Data.Bitraversable (bitraverse)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function (fix)
import Data.Functor.Base (TreeF(..))
import Text.Megaparsec (customFailure)
import Data.Monoid (All(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Foldable (Recursive(..), Base)
import Data.Functor.Foldable.Monadic (cataM)
import Data.Functor.Identity (Identity(..))
import Lens.Micro (traverseOf)
import Lens.Micro.Extras (view)

-- Expands all selections into recursive trees of fields, while validating that:
-- - Document doesn't have unused fragments
-- - Fragments don't form cycles
-- - Fragment spreads are valid
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
  frags <- traverseFrag (fragments doc) . fragments $ doc
  ops   <- traverseOp frags . operations $ doc
  pure $ Document frags ops
  where
    traverseFrag frags = traverseOf (traverse . _fragSelection) $ traverseAccum (eraseFragments $ visitFragment frags)
    traverseOp   frags = traverseOf (traverse . _opSelection) $ traverseAccum (eraseFragments $ pickFragment frags)

eraseFragments
  :: Monad f
  => (Pos -> Name -> f (NonEmpty (Tree a)))
  -> Selection a
  -> f (NonEmpty (Tree a))
eraseFragments f (pos :< Node a as          ) = pure . (pos :<) . NodeF a <$> traverseAccum (fmap NE.toList . eraseFragments f) as
eraseFragments f (_   :< InlineFragment _ as) = traverseAccum (eraseFragments f) as
eraseFragments f (pos :< FragmentSpread k   ) = f pos k

pickFragment :: HashMap Name (Fragment a) -> Pos -> Name -> StateT (Set Name) V (NonEmpty a)
pickFragment frags pos k = do
  ST.modify (Set.insert k)
  case HashMap.lookup k frags of
    Just frag -> pure $ fragSelection frag
    Nothing -> lift $ E.validationError [pos] $ "Fragment " <> k <> " is not defined"

visitFragment :: HashMap Name (Fragment (Selection a)) -> Pos -> Name -> StateT (Set Name) V (NonEmpty (Tree a))
visitFragment frags = fix $ \f pos k -> do
  visited <- ST.get
  if Set.member k visited
    then lift $ E.validationError [pos] $ "Cycle in fragment " <> k
    else case HashMap.lookup k frags of
      Just frag -> do
        ST.put (Set.insert k visited)
        traverseAccum (eraseFragments f) $ fragSelection frag
      Nothing   -> lift $ E.validationError [pos] $ "Fragment " <> k <> " is not defined"

-- Extracts one operation and returns an executable operation, and performs the following validations:
-- - Operation name input is valid
-- - No required variables are missing
getExecutableOperation :: JSON.Object -> Maybe Name -> Document (Tree (Field Value)) -> V (Operation (Tree (Field JSON.Value)))
getExecutableOperation input opName doc = do
  op <- getOperation opName doc
  vars <- HashMap.traverseWithKey (resolveVariable input) . view _opVariables $ op
  traverse (hoistCofreeM $ bitraverse (traverse $ resolveValue vars) pure) op

getOperation :: Maybe Name -> Document a -> V (Operation a)
getOperation Nothing     (Document _ (L1 (Identity op))) = pure op
getOperation Nothing     (Document _ (R1 _            )) = E.validationError [] "Operation name is required for documents with multiple operations"
getOperation (Just name) (Document _ (L1 (Identity op)))
  | opName op == Just name = pure op
  | otherwise = E.validationError [] $ "Operation " <> name <> " is not defined"
getOperation (Just name) (Document _ (R1 ops)) = case HashMap.lookup name ops of
  Nothing -> E.validationError [] $ "Operation " <> name <> " is not defined"
  Just op -> pure op

resolveValue :: HashMap Name (Variable, JSON.Value) -> Value -> V JSON.Value
resolveValue vars = cataM alg
  where
    alg :: Base Value JSON.Value -> V JSON.Value
    alg (pos CofreeT.:< R1 val) = pure (JSON.toJSON val)
    alg (pos CofreeT.:< L1 (Const k)) = case HashMap.lookup k vars of
      Nothing -> E.validationError [pos] $ "Variable $" <> k <> " is not defined"
      Just (_,val) -> pure val

resolveVariable :: JSON.Object -> Name -> Variable -> V (Variable, JSON.Value)
resolveVariable = HashMap.filter (/= JSON.Null) >>> \input k var ->
  case HashMap.lookup k input of
    Just val -> pure (var, val)
    Nothing | Just val <- varValue var -> pure (var,cata (JSON.toJSON . CofreeT.tailF) val)
            | isNullable (varDefinition var) -> pure (var,JSON.Null)
            | otherwise -> E.validationError [varPos var] $ "Required variable $" <> k <> " is missing from input"

-- Validates a document's immediate children
validateDocumentP :: ([Fragment a], [Operation a]) -> Parser (Document a)
validateDocumentP (frags, ops) = Document <$> validateFragmentsP frags <*> validateOperationsP ops

-- Validates that each document either has only one operation or all named operations
validateOperationsP :: [Operation a] -> Parser ((Identity :+: HashMap Name) (Operation a))
validateOperationsP [] = parseErrorP [] "Expected at least one root operation, found none"
validateOperationsP [op] = pure $ L1 $ pure op
validateOperationsP ops = R1 <$> (sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) =<< mapM go ops)
  where
    go op | Just name <- opName op = pure (name, pure op)
          | otherwise = validationErrorP [opPos op] "Unnamed operation in document with multiple operations"
    onDupe k op0 op1 = validationErrorP [opPos op0, opPos op1] $ "Duplicated operation name " <> k

-- Validates that fragment names are unique
validateFragmentsP :: [Fragment a] -> Parser (HashMap Name (Fragment a))
validateFragmentsP = sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) . fmap (fragName &&& pure)
  where
    onDupe k frag0 frag1 = validationErrorP [fragPos frag0, fragPos frag1] $ "Duplicated fragment name " <> k

parseErrorP :: [Pos] -> Text -> Parser a
parseErrorP pos msg = customFailure $ E.ParseError pos msg

validationErrorP :: [Pos] -> Text -> Parser a
validationErrorP pos msg = customFailure $ E.ValidationError pos msg

traverseAccum :: (Monad m, Traversable f, Monad f) => (a -> m (f b)) -> f a -> m (f b)
traverseAccum f = pure . join <=< traverse f

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb = join (liftA2 f ma mb)
