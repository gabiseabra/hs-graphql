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
import GraphQL.Internal

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

resolve :: forall a m t
  .  Monad m
  => Recursive t
  => Base t ~ TreeF Selection
  => GraphQLOutputType m a
  => [t]
  -> V (a -> m JSON.Value)
resolve = fun
  where
    go :: forall f a. Step t m a -> V (a -> m JSON.Value)
    go (Step f s) = pure . (\g -> f >=> g) =<< fun s
    fun :: forall a. GraphQLOutputType m a => [t] -> V (a -> m JSON.Value)
    fun s = sequenceResolver go =<< validate s (mkResolver (typeOf_ @a))

sequenceResolver
  :: Monad m
  => (forall a. f a -> V (a -> m JSON.Value))
  -> Resolver k f a
  -> V (a -> m JSON.Value)
sequenceResolver _ Leaf = pure $ pure . JSON.toJSON
-- TODO — if it's an branch wrapper, batch together all the same fields
sequenceResolver f (Wrap r) = pure . g =<< sequenceResolver f r
  where g f = pure . JSON.toJSON1 <=< traverse f
sequenceResolver f (Branch as) = pure . g =<< mapM f as
  where g as = pure . JSON.Object <=< sequence2 as

sequence2 :: (Traversable t, Monad m, Monad f) => t (f (m a)) -> f (m (t a))
sequence2 = fmap sequence . sequence

validate
  :: Recursive t
  => Base t ~ TreeF Selection
  => [t]
  -> Resolver k (Field m) a
  -> V (Resolver k (Step t m) a)
validate [] Leaf = pure Leaf
validate s (Wrap r) = pure . Wrap =<< validate s r
validate s@(_:_) (Branch as) = pure . Branch . Map.fromList =<< mapM (f . project) s
  where
    f (NodeF (Sel { name, alias, input }) tail)
      = select name as
      >>= apply tail input
      >>= \r -> pure (fromMaybe name alias, r)
validate _ _ = Left "Invalid selection"

select :: Text -> HashMap Text a -> V a
select k as = case Map.lookup k as of
  Nothing -> Left $ "field " <> Text.unpack k <> " doesn't exist"
  Just a -> Right a

apply :: [t] -> Input -> Field m a -> V (Step t m a)
apply s i (Field f) = (\i -> pure $ Step (\a -> f a i) s) =<< readInput i

