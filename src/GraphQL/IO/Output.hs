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
#-}

module GraphQL.IO.Output where

import GraphQL.Class
import GraphQL.Internal
import GraphQL.IO.Input

import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Row (Rec, Var)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

data NodeType = LEAF | BRANCH | VARIANT | WRAPPER NodeType

type family NodeTypeOf k where
  NodeTypeOf GQL_SCALAR = LEAF
  NodeTypeOf GQL_ENUM = LEAF
  NodeTypeOf GQL_OBJECT = BRANCH
  NodeTypeOf GQL_UNION = VARIANT
  NodeTypeOf (GQL_LIST k) = WRAPPER (NodeTypeOf k)
  NodeTypeOf (GQL_NULLABLE k) = WRAPPER (NodeTypeOf k)

data Case f a where Case :: (a -> Maybe b) -> f b -> Case f a

data Resolver t f a where
  Leaf :: JSON.ToJSON a => Resolver LEAF f a
  Branch :: HashMap Text (f a) -> Resolver BRANCH f a
  Variant :: HashMap Typename (Case (Resolver BRANCH f) a) -> Resolver VARIANT f a
  Wrap ::
    ( Traversable f
    , Functor f
    , JSON.ToJSON1 f
    ) => Resolver t f' a
      -> Resolver (WRAPPER t) f' (f a)

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
  , (KIND t) !>> OUT
  ) => GraphQLOutputKind (m :: * -> *) (t :: * -> *) where
  mkResolver :: t a -> Resolver (NodeTypeOf (KIND t)) (Field m) a

class
  ( GraphQLOutputKind m (KindOf a)
  , GraphQLType a
  ) => GraphQLOutputType m a
instance
  ( GraphQLOutputKind m (KindOf a)
  , GraphQLType a
  ) => GraphQLOutputType m a
