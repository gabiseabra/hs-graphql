{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , FlexibleContexts
  , ConstraintKinds
  , MultiParamTypeClasses
  , TypeOperators
  , RankNTypes
#-}

module GraphQL.Class
  ( Typename
  , TypeKind(..)
  , TypeIO(..)
  , type (?>>)
  , type (!>>)
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
  = GQL_SCALAR
  | GQL_ENUM
  | GQL_OBJECT
  | GQL_INPUT_OBJECT
  | GQL_UNION
  | GQL_LIST TypeKind
  | GQL_NULLABLE TypeKind

data TypeIO = IN | OUT

type family k ?>> io where
  GQL_SCALAR       ?>> io  = True
  GQL_ENUM         ?>> io  = True
  GQL_UNION        ?>> OUT = True
  GQL_OBJECT       ?>> OUT = True
  GQL_INPUT_OBJECT ?>> IN  = True
  (k' k)           ?>> io  = k ?>> io
  k                ?>> io  = False

type k !>> io = k ?>> io ~ True

class GraphQLKind (t :: * -> *) where type KIND t :: TypeKind

class GraphQLTypeable t a where typeOf :: t a

typeOf_ :: forall a t. InstanceOf t a => t a
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
