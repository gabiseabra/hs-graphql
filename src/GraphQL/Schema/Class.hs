{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module GraphQL.Schema.Class where

import           Control.Arrow                  ( Kleisli )
import           Control.Lens                   ( (<&>)
                                                , Lens'
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Functor.Identity          ( Identity )
import           Data.Map.Strict                ( Map )
import qualified Data.Row                      as Row
import           Data.Row                       ( Rec
                                                , Row
                                                , Var
                                                )
import qualified Data.Row.Records              as Rec
import           Data.Text                      ( Text )
import           GHC.Base                       ( Alternative )
import           GHC.Exts                       ( Constraint )
import           GraphQL.Internal

type Typename = Text

data OperationType
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  deriving (Eq, Show)

data TypeKind where
  SCALAR       ::                         TypeKind
  ENUM         ::                         TypeKind
  INPUT_OBJECT ::                         TypeKind
  UNION        :: forall (m :: * -> *)  . TypeKind
  OBJECT       :: forall (m :: * -> *)  . TypeKind
  LIST         :: TypeKind             -> TypeKind
  NULLABLE     :: TypeKind             -> TypeKind
  -- A pseudo-kind (which doesn't represent a real GraphQL type) of pure types
  -- that unify with UNION/OBJECT kinds applied to any type `m`.
  PURE         :: TypeKind              -> TypeKind

type PURE_OBJECT = PURE (OBJECT @Identity) :: TypeKind
type PURE_UNION = PURE (UNION @Identity) :: TypeKind

data TypeIO = IN | OUT

type family k ?>> io where
  SCALAR       ?>> io  = True
  ENUM         ?>> io  = True
  INPUT_OBJECT ?>> IN  = True
  UNION  @m    ?>> OUT = True
  OBJECT @m    ?>> OUT = True
  (k' k)       ?>> io  = k ?>> io
  k            ?>> io  = False

-- A constraint for checking that the type kind on the left may be used in the
-- location on the right ([IN]put or [OUT]put).
type k !>> io = k ?>> io ~ True

type family k || m where
  SCALAR        || m = m
  ENUM          || m = m
  INPUT_OBJECT  || m = m
  UNION   @m    || _ = m
  OBJECT  @m    || _ = m
  PURE     k    || m = m
  (k' k)        || m = k || m

-- A constraint for checking that the type kind on the left may be instantiated
-- with effects of the type (* -> *) on the right.
type k !! m = k || m ~ m

-- A class for defining GraphQL types.
class GraphQLType a where
  type KIND a :: TypeKind
  typeDef :: TypeDef (KIND a) a

class (GraphQLType a, KIND a !>> IN) => GraphQLInputType a
instance (GraphQLType a, KIND a !>> IN) => GraphQLInputType a

class (GraphQLType a, KIND a !>> OUT, KIND a !! m) => GraphQLOutputType m a
instance (GraphQLType a, KIND a !>> OUT, KIND a !! m) => GraphQLOutputType m a

class GraphQLObjectKind m k
instance GraphQLObjectKind m (OBJECT @m)
instance GraphQLObjectKind m PURE_OBJECT

class (GraphQLOutputType m a) => GraphQLObjectType m a
instance (GraphQLOutputType m a, GraphQLObjectKind m (KIND a)) => GraphQLObjectType m a

-- Inputs are objects containing many input-type values.
-- This is a separate class from GraphQLType because inputs are not proper
-- GraphQL types; Although they work just like input objects, they don't have a
-- corresponding type definition in the schema introspection document.
class GraphQLInput a where
  inputDef :: InputDef a
  default inputDef
    :: Rec.ToNative a
    => Row.AllUniqueLabels (Rec.NativeRow a)
    => Row.Forall (Rec.NativeRow a) GraphQLInputType
    => Row.FreeForall (Rec.NativeRow a)
    => InputDef a
  inputDef = InputFields Rec.toNative

instance GraphQLInput () where
  inputDef = EmptyFields ()

-- Carries information on how to decode an input type
data InputDef a where
  EmptyFields ::a -> InputDef a
  InputFields ::( Row.Forall r GraphQLInputType
    , Row.AllUniqueLabels r
    ) => (Rec r -> a)
      -> InputDef a

-- * Type definitions & schema introspection

data EnumValue a = EnumValue
  { enumValueDescription :: Maybe Text
  , enumValue            :: a
  }
  deriving Functor

data Field f i a = Field
  { fieldDescription :: Maybe Text
  , fieldResolver    :: i -> f a
  }
  deriving Functor

-- A field resolver is a function taking any instance of GraphQLInput as `input`
-- and a `target` object that owns the field, and returning a GraphQLOutputType
-- that resumes the resolution process:
-- Field (Kleisli m target) input output ~ input -> target -> m output
type Resolver m a
  = Exists2 (Field (Kleisli m a)) GraphQLInput (GraphQLOutputType m)

-- A variant is a resolver for union types; resolving union types requires testing
-- for each variant and is partial.
type Variant m a = Exists1 (Kleisli Maybe a) (GraphQLObjectType m)

-- Definitions of output types which carry information required for resolution
type TypeDef :: TypeKind -> * -> *
data TypeDef k a where
  ScalarType ::( JSON.ToJSON a
    , JSON.FromJSON a
    ) =>
    { scalarTypename :: Typename
    , scalarDescription :: Maybe Text
    } -> TypeDef SCALAR a
  EnumType ::{ enumTypename :: Typename
    , enumDescription :: Maybe Text
    , enumValues :: Map Text (EnumValue a)
    , encodeEnum :: a -> Text
    } -> TypeDef ENUM a
  InputObjectType ::( Row.Forall r GraphQLInputType
    , Row.AllUniqueLabels r
    ) =>
    { inputObjectTypename :: Typename
    , inputObjectDescription :: Maybe Text
    , decodeInputObject :: Rec r -> a
    } -> TypeDef INPUT_OBJECT a
  ObjectType ::{ objectTypename :: Typename
    , objectDescription :: Maybe Text
    , objectFields :: Map Text (Resolver m a)
    } -> TypeDef (OBJECT @m) a
  UnionType ::{ unionTypename :: Typename
    , unionDescription :: Maybe Text
    , unionPossibleTypes :: Map Typename (Variant m a)
    } -> TypeDef (UNION @m) a
  PureType ::( k !! Identity
    ) =>
    { pureInnerType :: TypeDef k a
    } -> TypeDef (PURE k) a
  ListType ::( JSON.ToJSON1 f
    , JSON.FromJSON1 f
    , Traversable f
    ) =>
    { listTypename :: Typename
    , listDescription :: Maybe Text
    , listInnerType :: TypeDef k a
    } -> TypeDef (LIST k) (f a)
  NullableType ::( JSON.ToJSON1 f
    , JSON.FromJSON1 f
    , Traversable f
    , Alternative f
    ) =>
    { nullableTypename :: Typename
    , nullableDescription :: Maybe Text
    , nullableInnerType :: TypeDef k a
    } -> TypeDef (NULLABLE k) (f a)

_typename :: Lens' (TypeDef k a) Typename
_typename f t = f (get t) <&> set t
 where
  get :: TypeDef k a -> Typename
  get ScalarType {..}      = scalarTypename
  get EnumType {..}        = enumTypename
  get ObjectType {..}      = objectTypename
  get InputObjectType {..} = inputObjectTypename
  get UnionType {..}       = unionTypename
  get PureType {..}        = get pureInnerType
  get ListType {..}        = listTypename
  get NullableType {..}    = nullableTypename

  set :: TypeDef k a -> Typename -> TypeDef k a
  set def@ScalarType {..}      a = def { scalarTypename = a }
  set def@EnumType {..}        a = def { enumTypename = a }
  set def@ObjectType {..}      a = def { objectTypename = a }
  set def@InputObjectType {..} a = def { inputObjectTypename = a }
  set def@UnionType {..}       a = def { unionTypename = a }
  set def@PureType {..}        a = PureType $ set pureInnerType a
  set def@ListType {..}        a = def { listTypename = a }
  set def@NullableType {..}    a = def { nullableTypename = a }

_description :: Lens' (TypeDef k a) (Maybe Text)
_description f t = f (get t) <&> set t
 where
  get :: TypeDef k a -> Maybe Text
  get ScalarType {..}      = scalarDescription
  get EnumType {..}        = enumDescription
  get InputObjectType {..} = inputObjectDescription
  get ObjectType {..}      = objectDescription
  get UnionType {..}       = unionDescription
  get PureType {..}        = get pureInnerType
  get ListType {..}        = listDescription
  get NullableType {..}    = nullableDescription

  set :: TypeDef k a -> Maybe Text -> TypeDef k a
  set def@ScalarType {..}      a = def { scalarDescription = a }
  set def@EnumType {..}        a = def { enumDescription = a }
  set def@InputObjectType {..} a = def { inputObjectDescription = a }
  set def@ObjectType {..}      a = def { objectDescription = a }
  set def@UnionType {..}       a = def { unionDescription = a }
  set def@PureType {..}        a = PureType $ set pureInnerType a
  set def@ListType {..}        a = def { listDescription = a }
  set def@NullableType {..}    a = def { nullableDescription = a }

kindOf :: forall k a . TypeDef k a -> String
kindOf ScalarType{}      = "SCALAR"
kindOf EnumType{}        = "ENUM"
kindOf InputObjectType{} = "INPUT_OBJECT"
kindOf ObjectType{}      = "OBJECT"
kindOf UnionType{}       = "UNION"
kindOf PureType {..}     = kindOf pureInnerType
kindOf ListType {..}     = "LIST " <> kindOf listInnerType
kindOf NullableType {..} = "NULLABLE " <> kindOf nullableInnerType
