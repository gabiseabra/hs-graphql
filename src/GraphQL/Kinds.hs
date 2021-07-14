{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module GraphQL.Kinds
  ( GraphQLScalar(..)
  , GraphQLObject(..)
  ) where

import GraphQL.Internal (mapRow)
import GraphQL.Class
import GraphQL.IO.Output
import GraphQL.IO.Input

import GHC.Exts (Constraint)

import Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
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
  readInputType Scalar = JSON.fromJSON
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

data GraphQLInputObject r a where
  InputObject ::
    ( Rec.ToNative a
    , Rec.NativeRow a ~ r
    , Row.AllUniqueLabels r
    , Row.Forall r IsInput
    ) => GraphQLInputObject r a

instance GraphQLKind (GraphQLInputObject r) where
  type Kind (GraphQLInputObject r) = INPUT_OBJECT
  typeDef InputObject = InputObjectDef $ mapRow @IsInput @r mkInputField
instance GraphQLInputKind (GraphQLInputObject r) where
  readInputType InputObject (JSON.Object obj) = return . Rec.toNative =<< Rec.fromLabelsA @IsInput @JSON.Result @r readField
    where
      readField :: forall l a. (Row.KnownSymbol l, IsInput a) => Row.Label l -> JSON.Result a
      readField l =
        let
          key = Text.pack (show l)
          val = fromMaybe JSON.Null $ Map.lookup key obj
        in readInput val
  readInputType InputObject _ = JSON.Error "input value is not an object"
