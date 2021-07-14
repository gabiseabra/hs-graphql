{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GraphQL.Types where

import GraphQL.Class
import GraphQL.Kinds

import Data.Text (Text)

instance GraphQLType Bool where
  type KindOf Bool = GraphQLScalar
  typeOf = Scalar
  typename _ = "Boolean"

instance GraphQLType Int where
  type KindOf Int = GraphQLScalar
  typeOf = Scalar
  typename _ = "Int"

instance GraphQLType Integer where
  type KindOf Integer = GraphQLScalar
  typeOf = Scalar
  typename _ = "Int"

instance GraphQLType Float where
  type KindOf Float = GraphQLScalar
  typeOf = Scalar
  typename _ = "Float"

instance GraphQLType Double where
  type KindOf Double = GraphQLScalar
  typeOf = Scalar
  typename _ = "Float"

instance GraphQLType [Char] where
  type KindOf [Char] = GraphQLScalar
  typeOf = Scalar
  typename _ = "String"

instance GraphQLType Text where
  type KindOf Text = GraphQLScalar
  typeOf = Scalar
  typename _ = "String"
