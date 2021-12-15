{-# LANGUAGE
    TypeFamilies
  , DataKinds
  , OverloadedStrings
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
  , UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeOperators
  , StandaloneKindSignatures
  , AllowAmbiguousTypes
  , PolyKinds
#-}

module GraphQL.Types where

import GraphQL.TypeSystem

import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import qualified Data.Aeson as JSON
import Data.Proxy (Proxy(..))
import Data.Row (type (.+), type (.==), type (.!))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as Text

newtype ID = ID String deriving (JSON.ToJSON, JSON.FromJSON)

instance GraphQLType ID where
  type KIND ID = SCALAR
  typeDef = scalarDef "ID"

instance GraphQLType Bool where
  type KIND Bool = SCALAR
  typeDef = scalarDef "Boolean"

instance GraphQLType Int where
  type KIND Int = SCALAR
  typeDef = scalarDef "Int"

instance GraphQLType Integer where
  type KIND Integer = SCALAR
  typeDef = scalarDef "Int"

instance GraphQLType Float where
  type KIND Float = SCALAR
  typeDef = scalarDef "Float"

instance GraphQLType Double where
  type KIND Double = SCALAR
  typeDef = scalarDef "Float"

instance GraphQLType Text where
  type KIND Text = SCALAR
  typeDef = scalarDef "String"

instance GraphQLType Char where
  type KIND Char = SCALAR
  typeDef = scalarDef "String"

instance GraphQLType a => GraphQLType (Maybe a) where
  type KIND (Maybe a) = NULLABLE (KIND a)
  typeDef = NullableType "Maybe" Nothing (typeDef @a)

instance GraphQLType a => GraphQLType (Vector a) where
  type KIND (Vector a) = LIST (KIND a)
  typeDef = ListType "Vector" Nothing (typeDef @a)

instance GraphQLType a => GraphQLType (NonEmpty a) where
  type KIND (NonEmpty a) = LIST (KIND a)
  typeDef = ListType "NonEmpty" Nothing (typeDef @a)

type family StringOrListK a where
  StringOrListK String = "String"
  StringOrListK a      = "List"

type ListGraphQLType :: Symbol -> * -> Constraint
class ListGraphQLType sym a where
  type LIST_KIND sym a :: TypeKind
  listTypeDef :: TypeDef (LIST_KIND sym a) a

instance ListGraphQLType "String" String where
  type LIST_KIND "String" String = SCALAR
  listTypeDef = scalarDef "String"

instance GraphQLType a => ListGraphQLType "List" [a] where
  type LIST_KIND "List" [a] = LIST (KIND a)
  listTypeDef = ListType "List" Nothing (typeDef @a)

instance
  ( StringOrListK [a] ~ sym
  , KnownSymbol sym
  , ListGraphQLType sym [a]
  ) => GraphQLType [a] where
  type KIND [a] = LIST_KIND (StringOrListK [a]) [a]
  typeDef = listTypeDef @sym @[a]
