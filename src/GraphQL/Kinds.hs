{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module GraphQL.Kinds
  ( GraphQLScalar(..)
  , GraphQLObject(..)
  ) where

import GraphQL.Typeable
import GraphQL.IO.Output
import GraphQL.IO.Input

import GHC.Exts (Constraint)

import Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import Data.Functor.Const (Const(..))
import Data.Proxy (Proxy(..))
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import Data.Row.Internal (LT(..), Row(..), Label(..))
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Text (Text)

data GraphQLScalar a where
  Scalar ::
    ( JSON.FromJSON a
    , JSON.ToJSON a
    ) => GraphQLScalar a

instance GraphQLKind GraphQLScalar where
  type Kind GraphQLScalar = SCALAR
  typeDef Scalar = ScalarDef
instance GraphQLInputKind GraphQLScalar where
  readInputType Scalar v = case JSON.fromJSON v of
    JSON.Error e -> Left (Text.pack e)
    JSON.Success a -> Right a
instance GraphQLOutputKind m GraphQLScalar where
  resolve Scalar = Leaf . JSON.toJSON

data GraphQLObject m r a where
  Object ::
    ( Rec.FromNative a
    , Rec.NativeRow a ~ r
    , Row.AllUniqueLabels r
    , Row.Forall r (IsResolver m)
    ) => GraphQLObject m r a

instance GraphQLKind (GraphQLObject m r) where
  type Kind (GraphQLObject m r) = OBJECT
  typeDef Object = ObjectDef $ mapRow @(IsResolver m) @r mkField
instance GraphQLOutputKind m (GraphQLObject m r) where
  resolve Object = Branch . Rec.eraseWithLabels @(IsResolver m) @r mkResolver . Rec.fromNative
mapRow :: forall c r b
  .  Row.Forall r c
  => Row.AllUniqueLabels r
  => (forall proxy l a. (Row.KnownSymbol l, c a) => Row.Label l -> proxy a -> b)
  -> [b]
mapRow f = getConst $ Rec.fromLabelsA @c @(Const [b]) @r g
  where
    g :: forall l a. (Row.KnownSymbol l, c a) => Row.Label l -> Const [b] a
    g l = Const [f l (Proxy @a)]
