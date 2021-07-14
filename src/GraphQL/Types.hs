{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module GraphQL.Types where

import GraphQL.Typeable
import GraphQL.IO.Output
import GraphQL.IO.Input

import qualified Data.Aeson as JSON
import qualified Data.Row.Records as Rec

import Data.Text (Text)

data GraphQLScalar a where
  Scalar ::
    ( JSON.FromJSON a
    , JSON.ToJSON a
    ) => GraphQLScalar a

data GraphQLEnum a where
  Enum ::
    ( JSON.FromJSON a
    , JSON.ToJSON a
    ) => GraphQLEnum a

data GraphQLObject m r a where
  Object ::
    ( Rec.FromNative a
    , Rec.NativeRow a ~ r
    , Rec.Forall r (IsResolver m)
    ) => GraphQLObject m r a
