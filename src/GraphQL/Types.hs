{-# LANGUAGE
    TypeFamilies
  , DataKinds
  , FlexibleInstances
  , OverloadedStrings
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
  , FlexibleContexts
  , UndecidableInstances
  , MultiParamTypeClasses
  , QuantifiedConstraints
  , TypeFamilyDependencies
#-}

module GraphQL.Types where

import GraphQL.Class
import GraphQL.Kinds
import GraphQL.IO.Output
import GraphQL.IO.Input

import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.Void (Void)
import Data.Proxy (Proxy(..))

newtype ID = ID String deriving (JSON.ToJSON, JSON.FromJSON)

instance GraphQLType ID where
  type KindOf ID = GraphQLScalar
  typename _ = "ID"

instance GraphQLType Bool where
  type KindOf Bool = GraphQLScalar
  typename _ = "Boolean"

instance GraphQLType Int where
  type KindOf Int = GraphQLScalar
  typename _ = "Int"

instance GraphQLType Integer where
  type KindOf Integer = GraphQLScalar
  typename _ = "Int"

instance GraphQLType Float where
  type KindOf Float = GraphQLScalar
  typename _ = "Float"

instance GraphQLType Double where
  type KindOf Double = GraphQLScalar
  typename _ = "Float"

instance GraphQLType Text where
  type KindOf Text = GraphQLScalar
  typename _ = "String"

instance GraphQLType a => GraphQLType (Maybe a) where
  type KindOf (Maybe a) = GraphQLNullable (KindOf a) Maybe
  typename _ = "Nullable"

type family IsString a where
  IsString String = True
  IsString a      = False

instance
  ( GraphQLListOrString (IsString [a]) (KindOf a) ~ t
  , GraphQLType a
  , GraphQLKind t
  , GraphQLTypeable t [a]
  ) => GraphQLType [a] where
  type KindOf [a] = GraphQLListOrString (IsString [a]) (KindOf a)
  typename _ = "Lmao"
