{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module GraphQL.IO.Output
  ( NodeType(..)
  , NodeTypeOf
  , Resolution(..)
  , Resolver(..)
  , IsResolver(..)
  , rootResolver
  , GraphQLOutputKind(..)
  ) where

import GraphQL.Class
import GraphQL.IO.Kinds
import GraphQL.IO.Input

import Data.Aeson (FromJSON, ToJSON(..), ToJSON1(..))
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor(..), bimap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Row as Row

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
    ( GraphQLType a
    , InstanceOf t a
    , GraphQLOutputKind m t
    -- , GraphQLInput i
    ) => (({-o-}) -> m a) -> Resolver m

rootResolver
  :: Monad m
  => GraphQLType a
  => GraphQLOutputKind m (KindOf a)
  => a
  -> Resolver m
rootResolver a = Resolver (\() -> return a)

class IsResolver (m :: * -> *) a | a -> m where
  type InputOf a :: *
  type OutputOf a :: *
  mkResolver :: a -> Resolver m
  mkField :: Row.KnownSymbol l => Row.Label l -> proxy a -> Field
instance
  ( GraphQLType a
  , InstanceOf t a
  , GraphQLOutputKind m t
  ) => IsResolver m (() -> m a) where
  type InputOf (() -> m a) = ()
  type OutputOf (() -> m a) = a
  mkResolver = Resolver
  mkField l proxy
    = Field
      { name = Text.pack (show l)
      , typeRep = TypeRep (typeOf @t @a)
      }

class
  ( GraphQLKind t
  , (Kind t) !>> OUT
  ) => GraphQLOutputKind (m :: * -> *) (t :: * -> *) where
  resolve :: t a -> a -> (Resolution (NodeTypeOf (Kind t)) JSON.Value (Resolver m))
