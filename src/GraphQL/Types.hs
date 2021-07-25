{-# LANGUAGE
    TypeFamilies
  , DataKinds
  , OverloadedStrings
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
  , UndecidableInstances
  , TypeOperators
#-}

module GraphQL.Types where

import GraphQL.Class
import GraphQL.Kinds

import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.Aeson as JSON
import Data.Proxy (Proxy(..))
import Data.Row (type (.+), type (.==))
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as Text

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
  type KindOf (Maybe a) = GraphQLNullable (KindOf a)
  typename _ = "Nullable"

type family StringOrListK a where
  StringOrListK String = "String"
  StringOrListK a      = "List"

type StringOrList t
  =  "String" .== GraphQLScalar
  .+ "List"   .== GraphQLList t

instance
  ( StringOrListK [a] ~ sym
  , KnownSymbol sym
  , GraphQLType a
  , GraphQLKind (sym .@ StringOrList (KindOf a))
  , GraphQLTypeable (sym .@ StringOrList (KindOf a)) [a]
  ) => GraphQLType [a] where
  type KindOf [a] = (StringOrListK [a] .@ StringOrList (KindOf a))
  typename _ = Text.pack $ symbolVal (Proxy @sym)
