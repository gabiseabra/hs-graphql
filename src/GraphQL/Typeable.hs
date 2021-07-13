{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GraphQL.Typeable where

import Data.Text (Text)

type Typename = Text

data TypeKind
  = SCALAR
  | ENUM
  | UNION
  | OBJECT
  | NULLABLE TypeKind
  | LIST TypeKind

data TypeRep where
  Rep :: InstanceOf t a => t a -> TypeRep

type EnumValue = Text

type PossibleType = TypeRep

type InnerType = TypeRep

data Field
  = Field
    { name :: String
    , typeRep :: TypeRep
    }

data InputField
  = InputField
    { name :: String
    , typeRep :: TypeRep
    }

data TypeDef k where
  ScalarDef :: TypeDef SCALAR
  EnumDef :: [EnumValue] -> TypeDef ENUM
  UnionDef :: [PossibleType] -> TypeDef UNION
  ObjectDef :: [Field] -> TypeDef OBJECT
  --ListDef :: InnerType -> TypeDef (LIST k)
  --NullableDef :: InnerType -> TypeDef (NULLABLE k)

class GraphQLType t where
  type KindOf t :: TypeKind
  typeDef :: InstanceOf t a => t a -> TypeDef (KindOf t)

class GraphQLType (TypeOf a) => GraphQLTypeable a where
  type TypeOf a :: * -> *
  typeOf :: (TypeOf a) a
  typename :: proxy a -> Typename
  description :: proxy a -> Maybe Text
  description _ = Nothing
  deprecationReason :: proxy a -> Maybe Text
  deprecationReason _ = Nothing

type InstanceOf t a
  = ( GraphQLTypeable a
    , GraphQLType t
    , TypeOf a ~ t
    )
