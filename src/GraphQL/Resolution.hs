{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , GADTs
  , DuplicateRecordFields
  , NamedFieldPuns
  , RankNTypes
  , BlockArguments
  , ScopedTypeVariables
  , TypeApplications
  , ConstraintKinds
#-}

module GraphQL.Resolution
  ( resolve
  ) where

import GraphQL.Class
import GraphQL.Selection
import GraphQL.IO.Output
import GraphQL.IO.Input
import GraphQL.Internal

import Control.Monad ((>=>), (<=<))
import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&), (<<<))
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.Bitraversable (bisequence)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable (Recursive(..), Corecursive(..), Base)
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Row (Rec)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec

data Step t m a where
  Step :: GraphQLOutputType m o => (a -> m o) -> [t] -> Step t m a

resolve :: forall a m t
  .  Monad m
  => SelectionR t
  => GraphQLOutputType m a
  => [t]
  -> V (a -> m JSON.Value)
resolve = fun
  where
    go :: forall f a. Step t m a -> V (a -> m JSON.Value)
    go (Step f s) = pure . (\g -> f >=> g) =<< fun s
    fun :: forall a. GraphQLOutputType m a => [t] -> V (a -> m JSON.Value)
    fun s = sequenceResolver go =<< validate s (mkResolver (typeOf_ @a))

sequenceResolver :: forall a m f k
  .  Monad m
  => (forall a. f a -> V (a -> m JSON.Value))
  -> Resolver k f a
  -> V (a -> m JSON.Value)
sequenceResolver _ Leaf = pure $ pure . JSON.toJSON
-- TODO — if it's an branch wrapper, batch together all the same fields
sequenceResolver f (Wrap r) = pure . g =<< sequenceResolver f r
  where g f = pure . JSON.toJSON1 <=< traverse f
sequenceResolver f (Branch as) = pure . g =<< mapM f as
  where g as = pure . JSON.Object <=< sequence2 as
sequenceResolver f (Variant as)
  = fmap ((<<<) (fromMaybe (pure $ JSON.Object Map.empty)))
  $ foldr ((liftA2 . liftA2) (<|>) . g) (pure $ const Nothing) as
  where g (Case h a) = pure . (. h) . fmap =<< sequenceResolver f a

sequence2 :: (Traversable t, Monad m, Monad f) => t (f (m a)) -> f (m (t a))
sequence2 = fmap sequence . sequence

validate :: forall a k m t
  .  SelectionR t
  => [t]
  -> Resolver k (Field m) a
  -> V (Resolver k (Step t m) a)
validate [] Leaf = pure Leaf
validate s (Wrap r) = pure . Wrap =<< validate s r
validate s@(_:_) (Branch as) = pure . Branch =<< validate'Object s as
validate s@(_:_) (Variant as) = pure . Variant =<< (flip validate'Variant) as =<< groupSelection s
validate _ _ = Left "Invalid selection"

validate'Object :: forall a m t
  .  SelectionR t
  => [t]
  -> HashMap Text (Field m a)
  -> V (HashMap Text (Step t m a))
validate'Object s as = pure . Map.fromList =<< mapM (f . project) s
  where
    f (NodeF (Sel { name, alias, input }) tail)
      = select name as
      >>= apply tail input
      >>= \r -> pure (fromMaybe name alias, r)

validate'Variant :: forall a m t
  .  SelectionR t
  => HashMap Typename [t]
  -> HashMap Typename (Case (Resolver BRANCH (Field m)) a)
  -> V (HashMap Typename (Case (Resolver BRANCH (Step t m)) a))
validate'Variant s = Map.traverseWithKey $ \k (Case f r) ->
  case Map.lookup k s of
    Nothing -> Case f <$> pure (Branch Map.empty)
    Just s' -> Case f <$> validate s' r

node :: TreeF a b -> a
node (NodeF a _) = a

groupSelection
  :: SelectionR t
  => [t]
  -> V (HashMap Typename [t])
groupSelection = pure . Map.fromListWith (++) <=< mapM go
  where
    go = maybe err pure . bisequence . (typeConstraint . node &&& Just . pure . embed) . project
    err = Left "No typename provided for union type"

select :: Text -> HashMap Text a -> V a
select k as = case Map.lookup k as of
  Nothing -> Left $ "field " <> Text.unpack k <> " doesn't exist"
  Just a -> Right a

apply :: [t] -> Input -> Field m a -> V (Step t m a)
apply s i (Field f) = (\i -> pure $ Step (\a -> f a i) s) =<< readInput i

type SelectionR t
  = ( Recursive t
    , Corecursive t
    , Base t ~ TreeF Selection
    )
