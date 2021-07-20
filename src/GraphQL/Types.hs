{-# LANGUAGE
    TypeFamilies
  , FlexibleInstances
  , OverloadedStrings
  , GeneralizedNewtypeDeriving
#-}

module GraphQL.Types where

import GraphQL.Class
import GraphQL.Kinds

import qualified Data.Aeson as JSON
import Data.Text (Text)

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

-- overlapping instance
-- instance GraphQLType [Char] where
--   type KindOf [Char] = GraphQLScalar
--   typename _ = "String"

instance GraphQLType Text where
  type KindOf Text = GraphQLScalar
  typename _ = "String"

instance GraphQLType a => GraphQLType [a] where
  type KindOf [a] = GraphQLList (KindOf a) []
  typename _ = "List"

instance GraphQLType a => GraphQLType (Maybe a) where
  type KindOf (Maybe a) = GraphQLNullable (KindOf a) Maybe
  typename _ = "Nullable"
