{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , GADTs
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  , DuplicateRecordFields
  , NamedFieldPuns
  , RankNTypes
  , BlockArguments
#-}

module GraphQL.Resolution where

import GraphQL.Class
import GraphQL.Selection
import GraphQL.IO.Output
import GraphQL.IO.Input

import Control.Monad ((<=<), liftM, liftM2)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor(..), bimap)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable (Recursive (..), Corecursive (..), Base)

{-
data ValidField m a where ValidField :: GraphQLOutputType m o => (a -> m o) -> ValidField m a

foldGQL :: forall a v m t
  .  MonadFail v
  => Recursive t
  => Base t ~ TreeF Selection
  => GraphQLOutputType m a
  => [t]
  -> v (a -> m JSON.Value)
foldGQL s = do
  let
    go :: forall f a. (ValidField m a, [t]) -> m (a -> m JSON.Value)
    go (ValidField f, s') = pure . (\g -> f >=> g) =<< foldGQL s'
    res = run (Proxy @a) :: Resolution a (R RAW m a)
  res' <- mapM go =<< resolve s res
  case res' of
    Leaf -> pure (pure . JSON.toJSON)
    (Wrap Leaf) -> pure (pure . JSON.toJSON1)
    (Branch as) -> do
      let fun = sequence2 (fmap sequence2 as) :: a -> m [(String, JSON.Value)]
      pure (pure . JSON.Object . Map.fromList . fmap (first Text.pack) <=< fun)

sequence2 :: (Traversable t, Monad m, Monad f) => t (f (m a)) -> f (m (t a))
sequence2 = fmap sequence . sequence

validate
  :: MonadFail v
  => Recursive t
  => Base t ~ TreeF Selection
  => [t]
  -> Resolver a' (Field m a)
  -> v (Resolver a' (ValidField m a, [t]))
validate [] Leaf = pure Leaf
validate s (Wrap r) = pure . Wrap =<< resolve s r
validate s@(_:_) (Branch as) = pure . Branch =<< mapM (f . project) s
  where f (NodeF (Sel k i) tail) = select k as >>= traverse ((flip apply) i) >>= \(k, r) -> pure (k, (r, tail))
validate _ _ = fail "Invalid selection"

select :: String -> [(String, a)] -> V (String, a)
select k as = case List.lookup k as of
  Nothing -> fail $ "field " <> k <> " doesn't exist"
  Just a -> pure (k, a)

apply :: MonadFail v => Field m a -> JSON.Object -> v (ValidField m a)
apply (R f) = readInput >=> \i -> pure $ ValidField \a -> f a i
-}
