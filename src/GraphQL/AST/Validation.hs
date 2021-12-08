{-# LANGUAGE
    OverloadedStrings
  , TypeOperators
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  , FlexibleContexts
  , BlockArguments
#-}

module GraphQL.AST.Validation
  ( basicRules
  , mergeOverlappingFields
  , validateDefaultValues
  , collectFields
  ) where

import GraphQL.AST.Document
import GraphQL.Response (V, Pos)
import qualified GraphQL.Response as E

import Control.Arrow ((>>>), (&&&))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..), unwrap)
import qualified Control.Comonad.Trans.Cofree as CofreeT
import Control.Applicative ((<|>))
import Control.Monad ((<=<), join, foldM)
import Control.Monad.Trans (lift)
import Control.Monad.State.Lazy (StateT(..))
import qualified Control.Monad.State.Lazy as ST
import Control.Lens.Fold (mapMOf_)
import Control.Lens.Traversal (traverseOf)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List as List
import Data.Fix (Fix(..))
import Data.Function (on, fix)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.Monadic (cataM)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Sum (Sum(..))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Rule a b = Document a -> V (Document b)
type Rule' a = Rule a a

forget :: Functor f => Cofree f a -> Fix f
forget = cata \(_ CofreeT.:< x) -> Fix x

node :: TreeF a b -> a
node (NodeF a _) = a

basicRules :: Eq a => Rule (Selection (Field a)) (Tree (Field a))
basicRules
  = validateDefaultValues
  <=< collectFields

-- Validates that overlapping fields are compatible and deduplicates selections
-- TODO link to spec
mergeOverlappingFields :: Eq a => Rule' (Tree (Field a))
mergeOverlappingFields = traverse . cataM $ \(pos CofreeT.:< NodeF a as) -> (pos:<) . NodeF a <$> foldM merge [] as
  where
    merge as a = case List.find (on overlaps (node . unwrap) a) as of
      Nothing -> pure (a:as)
      Just b | forget a == forget b -> pure as
      Just b -> E.validationError [extract a, extract b] "Conflicting overlapping fields"
    name f = fieldAlias f <|> Just (fieldName f)
    overlaps = (==) `on` name

-- Validates that variables' default values are valid
validateDefaultValues :: Rule' a
validateDefaultValues doc = do
  mapMOf_ (_docOperations . traverse . _opVariables . traverse) validation doc
  pure doc
  where
    validation var@(Variable _ _ Nothing) = pure ()
    validation var@(Variable _ def (Just (pos:<val))) =
      if checkTypeDefinition def (pos:<val)
        then pure ()
        else E.validationError [pos]
              $ "Expected "
              <> Text.pack (show def)
              <> ", found "
              <> (LazyText.toStrict . JSON.encodeToLazyText . cata (JSON.toJSON . CofreeT.tailF) $ (pos:<val))

-- Expands all selections into recursive trees of fields, while validating that:
-- - Document doesn't have unused fragments
-- - Fragments don't form cycles
-- - Fragment spreads are valid
collectFields :: Rule (Selection a) (Tree a)
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

traverseAccum :: (Monad m, Traversable f, Monad f) => (a -> m (f b)) -> f a -> m (f b)
traverseAccum f = pure . join <=< traverse f
