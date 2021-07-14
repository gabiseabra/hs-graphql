{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module GraphQL.IO.Output where

import GraphQL.Typeable
import GraphQL.IO.Kinds
import GraphQL.IO.Input

import Data.Aeson (FromJSON, ToJSON(..), ToJSON1(..))
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor(..), bimap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text

data NodeType = LEAF | BRANCH | WRAPPER NodeType

type family NodeTypeOf k where
  NodeTypeOf SCALAR = LEAF
  NodeTypeOf ENUM = LEAF
  NodeTypeOf OBJECT = BRANCH
  NodeTypeOf UNION = BRANCH
  NodeTypeOf (LIST k) = WRAPPER (NodeTypeOf k)
  NodeTypeOf (NULLABLE k) = WRAPPER (NodeTypeOf k)

data Resolution n a r where
  Leaf :: a -> Resolution LEAF a r
  Branch :: [(String, r)] -> Resolution BRANCH a r
  Wrapper ::
    ( Traversable f
    , Functor f
    , ToJSON1 f
    ) => f (Resolution n a r)
      -> Resolution (WRAPPER n) a r

instance Functor (Resolution n a) where
  fmap f (Leaf a) = Leaf a
  fmap f (Branch r) = Branch $ fmap (fmap f) r
  fmap f (Wrapper r) = Wrapper $ fmap (fmap f) r
instance Foldable (Resolution n a) where
  foldMap f (Leaf _) = mempty
  foldMap f (Branch r) = foldMap (\(k, v) -> f v) r
  foldMap f (Wrapper r) = foldMap (foldMap f) r
instance Traversable (Resolution n a) where
  traverse f (Leaf a) = pure (Leaf a)
  traverse f (Branch r) = fmap Branch (traverse (traverse f) r)
  traverse f (Wrapper r) = fmap Wrapper (traverse (traverse f) r)
instance Bifunctor (Resolution n) where
  bimap f _ (Leaf a) = Leaf (f a)
  bimap _ g (Branch r) = Branch (fmap (fmap g) r)
  bimap f g (Wrapper r) = Wrapper (fmap (bimap f g) r)
instance ToJSON a => ToJSON (Resolution n a a) where
  toJSON (Leaf a) = toJSON a
  toJSON (Branch r) = JSON.Object (Map.fromList (fmap (bimap Text.pack toJSON) r))
  toJSON (Wrapper r) = liftToJSON toJSON toJSON r

data Resolver m where
  Resolver ::
    ( GraphQLTypeable a
    , GraphQLOutputType m (TypeOf a)
    -- , GraphQLInput i
    ) => (({-o-}) -> m a) -> Resolver m

rootResolver
  :: Monad m
  => GraphQLTypeable a
  => GraphQLOutputType m (TypeOf a)
  => a
  -> Resolver m
rootResolver a = Resolver (\() -> return a)

class IsResolver (m :: * -> *) a | a -> m where
  mkResolver :: a -> Resolver m
instance
  ( GraphQLTypeable a
  , GraphQLOutputType m (TypeOf a)
  --, GraphQLInput i
  ) => IsResolver m (({-i-}) -> m a) where
  mkResolver = Resolver

class
  ( GraphQLType t
  , (KindOf t) !>> OUT
  ) => GraphQLOutputType (m :: * -> *) (t :: * -> *) where
  resolve :: t a -> a -> (Resolution (NodeTypeOf (KindOf t)) JSON.Value (Resolver m))
