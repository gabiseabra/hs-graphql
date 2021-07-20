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
#-}

module GraphQL.Resolution
  ( resolve
  ) where

import GraphQL.Class
import GraphQL.Selection
import GraphQL.IO.Output
import GraphQL.IO.Input

import Control.Monad ((>=>), (<=<))
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable (Recursive(..), Corecursive(..), Base)
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text

data Step t m a where
  Step :: GraphQLOutputType m o => (a -> m o) -> [t] -> Step t m a

resolve :: forall a v m t
  .  MonadFail v
  => Monad m
  => Recursive t
  => Base t ~ TreeF Selection
  => GraphQLOutputType m a
  => [t]
  -> v (a -> m JSON.Value)
resolve = fun
  where
    go :: forall f a. Step t m a -> v (a -> m JSON.Value)
    go (Step f s) = pure . (\g -> f >=> g) =<< fun s
    fun :: forall a. GraphQLOutputType m a => [t] -> v (a -> m JSON.Value)
    fun s = sequenceResolver go =<< validate s (mkResolver (typeOf_ @a))

sequenceResolver
  :: Monad m
  => MonadFail v
  => (forall a. f a -> v (a -> m JSON.Value))
  -> Resolver k f a
  -> v (a -> m JSON.Value)
sequenceResolver _ Leaf = pure $ pure . JSON.toJSON
-- TODO — if it's an branch wrapper, batch together all the same fields
sequenceResolver f (Wrap r) = pure . g =<< sequenceResolver f r
  where g f = pure . JSON.toJSON1 <=< traverse f
sequenceResolver f (Branch as) = pure . g =<< mapM f as
  where g as = pure . JSON.Object <=< sequence2 as

sequence2 :: (Traversable t, Monad m, Monad f) => t (f (m a)) -> f (m (t a))
sequence2 = fmap sequence . sequence

validate
  :: MonadFail v
  => Recursive t
  => Base t ~ TreeF Selection
  => [t]
  -> Resolver k (Field m) a
  -> v (Resolver k (Step t m) a)
validate [] Leaf = pure Leaf
validate s (Wrap r) = pure . Wrap =<< validate s r
validate s@(_:_) (Branch as) = pure . Branch . Map.fromList =<< mapM (f . project) s
  where
    f (NodeF (Sel { name, alias, input }) tail)
      = select name as
      >>= apply tail input
      >>= \r -> pure (Text.pack $ fromMaybe name alias, r)
validate _ _ = fail "Invalid selection"

select :: MonadFail v => String -> HashMap Text a -> v a
select k as = case Map.lookup (Text.pack k) as of
  Nothing -> fail $ "field " <> k <> " doesn't exist"
  Just a -> pure a

apply :: MonadFail v => [t] -> Input -> Field m a -> v (Step t m a)
apply s i (Field f) = (\i -> pure $ Step (\a -> f a i) s) =<< readInput i

