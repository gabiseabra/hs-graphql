{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GraphQL.Typeable
  ( Typename
  , TypeKind(..)
  , TypeRep(..)
  , EnumValue
  , PossibleType
  , InnerType
  , Field(..)
  , InputField(..)
  , TypeDef(..)
  , GraphQLKind(..)
  , GraphQLType(..)
  , InstanceOf
  ) where

import Data.Text (Text)

type Typename = Text

data TypeKind
  = SCALAR
  | ENUM
  | UNION
  | OBJECT
  | INPUT_OBJECT
  | NULLABLE TypeKind
  | LIST TypeKind

data TypeRep where TypeRep :: InstanceOf t a => t a -> TypeRep

type EnumValue = Text

type PossibleType = TypeRep

type InnerType = TypeRep

data Field
  = Field
    { name :: Text
    -- , inputRep :: TypeRep
    , typeRep :: TypeRep
    }

data InputField
  = InputField
    { name :: Text
    , typeRep :: TypeRep
    }

data TypeDef k where
  ScalarDef :: TypeDef SCALAR
  EnumDef :: [EnumValue] -> TypeDef ENUM
  UnionDef :: [PossibleType] -> TypeDef UNION
  ObjectDef :: [Field] -> TypeDef OBJECT
  InputObjectDef :: [InputField] -> TypeDef INPUT_OBJECT
  --ListDef :: InnerType -> TypeDef (LIST k)
  --NullableDef :: InnerType -> TypeDef (NULLABLE k)

-- | Class of GraphQL type kinds
class GraphQLKind t where
  type Kind t :: TypeKind
  typeDef :: InstanceOf t a => t a -> TypeDef (Kind t)

-- | Class of GraphQL types
class GraphQLKind (KindOf a) => GraphQLType a where
  type KindOf a :: * -> *
  typeOf :: (KindOf a) a
  typename :: proxy a -> Typename
  description :: proxy a -> Maybe Text
  description _ = Nothing
  deprecationReason :: proxy a -> Maybe Text
  deprecationReason _ = Nothing

type InstanceOf t a
  = ( GraphQLType a
    , GraphQLKind t
    , KindOf a ~ t
    )
