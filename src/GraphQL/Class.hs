{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , FlexibleContexts
  , ConstraintKinds
  , MultiParamTypeClasses
#-}

module GraphQL.Class
  ( Typename
  , TypeKind(..)
  , GraphQLKind(..)
  , GraphQLTypeable(..)
  , typeOf_
  , GraphQLType(..)
  , InstanceOf
  ) where

import qualified Data.Aeson as JSON
import Data.Row (Rec, Row)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import Data.Text (Text)

type Typename = Text

data TypeKind
  = SCALAR
  | ENUM
  | OBJECT
  | INPUT_OBJECT
  | LIST TypeKind
  | NULLABLE TypeKind

class GraphQLKind (t :: * -> *) where type Kind t :: TypeKind

class GraphQLTypeable t a where typeOf :: t a

typeOf_ :: (GraphQLType a, InstanceOf t a) => t a
typeOf_ = typeOf

class
  ( GraphQLKind (KindOf a)
  , GraphQLTypeable (KindOf a) a
  ) => GraphQLType a where
  type KindOf a :: * -> *
  typename :: proxy a -> Typename
  description :: proxy a -> Maybe Text
  description _ = Nothing
  deprecationReason :: proxy a -> Maybe Text
  deprecationReason _ = Nothing

type InstanceOf t a
  = ( GraphQLType a
    , KindOf a ~ t
    )
