{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , GADTs
  , FunctionalDependencies
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  , TypeApplications
  , ScopedTypeVariables
  , TypeOperators
  , ConstraintKinds
#-}

module GraphQL.IO.Output where

import GraphQL.Class
import GraphQL.IO.Kinds
import GraphQL.IO.Input

import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Row as Row

data NodeType = LEAF | BRANCH | WRAPPER NodeType

type family NodeTypeOf k where
  NodeTypeOf SCALAR = LEAF
  NodeTypeOf ENUM = LEAF
  NodeTypeOf OBJECT = BRANCH
  NodeTypeOf (LIST k) = WRAPPER (NodeTypeOf k)
  NodeTypeOf (NULLABLE k) = WRAPPER (NodeTypeOf k)

data Resolver t a r where
  Leaf :: JSON.ToJSON a => Resolver LEAF a r
  Branch :: [(String, r)] -> Resolver BRANCH a r
  Wrap ::
    ( Traversable f
    , Functor f
    , JSON.ToJSON1 f
    ) => Resolver t a r
      -> Resolver (WRAPPER t) (f a) r

instance Functor (Resolver t a) where
  fmap f (Leaf) = Leaf
  fmap f (Branch r) = Branch $ fmap (fmap f) r
  fmap f (Wrap r) = Wrap $ fmap f r
instance Foldable (Resolver t a) where
  foldMap f (Leaf) = mempty
  foldMap f (Branch r) = foldMap (\(k, v) -> f v) r
  foldMap f (Wrap r) = foldMap f r
instance Traversable (Resolver t a) where
  traverse f (Leaf) = pure Leaf
  traverse f (Branch r) = fmap Branch (traverse (traverse f) r)
  traverse f (Wrap r) = fmap Wrap (traverse f r)

data Field m a where
  Field ::
    ( GraphQLOutputType m o
    , GraphQLInput i
    ) => (a -> i -> m o)
      -> Field m a

class
  ( GraphQLInput (InputOf a)
  , GraphQLOutputType m (OutputOf a)
  ) => GraphQLResolver m a | a -> m where
  type InputOf a :: *
  type OutputOf a :: *
  applyInput :: a -> InputOf a -> m (OutputOf a)
instance
  ( GraphQLInput i
  , GraphQLOutputType m a
  ) => GraphQLResolver m (i -> m a) where
  type InputOf (i -> m a) = i
  type OutputOf (i -> m a) = a
  applyInput = id

class
  ( GraphQLKind t
  , (Kind t) !>> OUT
  ) => GraphQLOutputKind (m :: * -> *) (t :: * -> *) where
  mkResolver :: t a -> Resolver (NodeTypeOf (Kind t)) a (Field m a)

class
  ( GraphQLOutputKind m (KindOf a)
  , GraphQLType a
  ) => GraphQLOutputType m a
instance
  ( GraphQLOutputKind m (KindOf a)
  , GraphQLType a
  ) => GraphQLOutputType m a
