{-# LANGUAGE
    DataKinds
  , GADTs
  , TypeFamilies
  , FlexibleContexts
  , ConstraintKinds
  , MultiParamTypeClasses
  , TypeOperators
  , RankNTypes
  , DefaultSignatures
  , AllowAmbiguousTypes
  , TypeApplications
  , ScopedTypeVariables
  , StandaloneKindSignatures
  , FlexibleInstances
  , UndecidableInstances
  , PolyKinds
#-}

module GraphQL.TypeSystem.Main where

import GraphQL.Internal
import GraphQL.Response

import GHC.Exts (Constraint)

import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty)
import Data.Vector (Vector)
import Data.Row (Row, Rec)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Proxy (Proxy)
import Data.Functor.Compose (Compose)
import Data.Profunctor.Cayley (Cayley)

type Typename = Text

data OperationType
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  deriving (Eq, Show)

data TypeKind where
  SCALAR       ::                                                TypeKind
  ENUM         ::                                                TypeKind
  INPUT_OBJECT ::                                                TypeKind
  OBJECT       :: forall (m :: * -> *)                         . TypeKind
  UNION        :: forall (m :: * -> *)                         . TypeKind
  LIST         :: TypeKind                                    -> TypeKind
  NULLABLE     :: TypeKind                                    -> TypeKind
  ROOT         :: forall (op :: OperationType) (m :: * -> *) r . TypeKind

data TypeIO = IN | OUT

type family k ?>> io where
  SCALAR       ?>> io  = True
  ENUM         ?>> io  = True
  INPUT_OBJECT ?>> IN  = True
  UNION  @m    ?>> OUT = True
  OBJECT @m    ?>> OUT = True
  (k' k)       ?>> io  = k ?>> io
  k            ?>> io  = False

type k !>> io = k ?>> io ~ True

type family k || m where
  SCALAR        || m = m
  ENUM          || m = m
  INPUT_OBJECT  || m = m
  UNION   @m    || _ = m
  OBJECT  @m    || _ = m
  ROOT @_ @m @_ || _ = m
  (k' k)        || m = k || m

type k !! m = k || m ~ m

class GraphQLType a where
  type KIND a :: TypeKind
  typeDef :: TypeDef (KIND a) a

class (GraphQLType a, KIND a !>> IN) => GraphQLInputType a
instance (GraphQLType a, KIND a !>> IN) => GraphQLInputType a

class (GraphQLType a, KIND a !>> OUT, KIND a !! m) => GraphQLOutputType m a
instance (GraphQLType a, KIND a !>> OUT, KIND a !! m) => GraphQLOutputType m a

class (GraphQLType a, KIND a ~ OBJECT @m) => GraphQLObjectType m a
instance (GraphQLType a, KIND a ~ OBJECT @m) => GraphQLObjectType m a

class (GraphQLType a, KIND a ~ ROOT @op @m @r) => GraphQLRootType op m r a
instance (GraphQLType a, KIND a ~ ROOT @op @m @r) => GraphQLRootType op m r a

class GraphQLInput a where
  inputDef :: InputDef a
  default inputDef
    :: Rec.ToNative a
    => Row.AllUniqueLabels (Rec.NativeRow a)
    => Row.Forall (Rec.NativeRow a) GraphQLInputType
    => Row.FreeForall (Rec.NativeRow a)
    => InputDef a
  inputDef = InputFields $ InputObjectDef Rec.toNative

instance GraphQLInput () where inputDef = EmptyFields ()

data InputDef a where
  EmptyFields :: a -> InputDef a
  InputFields :: InputObjectDef a -> InputDef a

-- * Type definitions & schema introspection

type TypeDef :: TypeKind -> * -> *
data TypeDef k a where
  ScalarType      :: Typename -> Maybe Text  -> ScalarDef        a -> TypeDef SCALAR       a
  EnumType        :: Typename -> Maybe Text  -> EnumDef          a -> TypeDef ENUM         a
  InputObjectType :: Typename -> Maybe Text  -> InputObjectDef   a -> TypeDef INPUT_OBJECT a
  ObjectType      :: Typename -> Maybe Text  -> ObjectDef      m a -> TypeDef (OBJECT @m)  a
  UnionType       :: Typename -> Maybe Text  -> UnionDef       m a -> TypeDef (UNION @m)   a
  ListType        :: Typename -> TypeDef k a -> ListDef          f -> TypeDef (LIST k)     (f a)
  NullableType    :: Typename -> TypeDef k a -> NullableDef      f -> TypeDef (NULLABLE k) (f a)
  RootType        :: Typename -> RootDef op m a r                  -> TypeDef (ROOT @op @m @r) a

typename :: TypeDef k a -> Typename
typename (ScalarType      ty _ _) = ty
typename (EnumType        ty _ _) = ty
typename (ObjectType      ty _ _) = ty
typename (InputObjectType ty _ _) = ty
typename (UnionType       ty _ _) = ty
typename (ListType        ty _ _) = ty
typename (NullableType    ty _ _) = ty
typename (RootType        ty _  ) = ty

type ScalarDef :: * -> *
data ScalarDef a
  = ScalarDef
    { encodeScalar :: a -> JSON.Value
    , decodeScalar :: JSON.Value -> Maybe a
    }

type EnumDef :: * -> *
data EnumDef a
  = EnumDef
    { enumValues :: NonEmpty EnumValueDef
    , encodeEnum :: a -> Text
    , decodeEnum :: Text -> Maybe a
    }

data EnumValueDef
  = EnumValueDef
    { value            :: Text
    , valueDescription :: Maybe Text
    }

type ObjectDef :: (* -> *) -> * -> *
newtype ObjectDef m a
  = ObjectDef
    { objectFields :: Map Text (Resolver m a)
    }

data FieldDef t m i a where
  FieldDef ::
    ( GraphQLInput i
    , GraphQLOutputType m a
    ) =>
    { fieldDescription :: Maybe Text
    , fieldResolver :: i -> t m a
    } -> FieldDef t m i a

data Some2 p where Some2 :: p a b -> Some2 p

runSome2 :: (forall a b. p a b -> r) -> Some2 p -> r
runSome2 f (Some2 p) = f p

type ResolverF m a i o = FieldDef (Compose ((->) a)) m i o
type Resolver m a = Some2 (FieldDef (Compose ((->) a)) m)

newtype ProducerT a r m o = Producer { runProducer :: a -> (o -> m Response) -> m r }
type ProducerF m a r i o = FieldDef (ProducerT a r) m i o
type Producer m a r = Some2 (FieldDef (ProducerT a r) m)

type InputObjectDef :: * -> *
data InputObjectDef a where
  InputObjectDef ::
    ( Row.Forall r GraphQLInputType
    , Row.AllUniqueLabels r
    ) =>
    { decodeInputObject :: Rec r -> a
    } -> InputObjectDef a

newtype InputValueDef a
  = InputValueDef
    { inputDescription :: Maybe Text
    }

type UnionDef :: (* -> *) -> * -> *
newtype UnionDef m a
  = UnionDef
    { unionTypes :: Map Typename (Case m a)
    }

data Case m a where Case :: GraphQLObjectType m b => (a -> Maybe b) -> Case m a

type ListDef :: (* -> *) -> *
data ListDef f where
  ListDef ::
    ( Traversable f
    ) =>
    { encodeList :: forall a. f a -> Vector a
    , decodeList :: forall a. Vector a -> Maybe (f a)
    } -> ListDef f

type NullableDef :: (* -> *) -> *
data NullableDef f where
  NullableDef ::
    ( Traversable f
    ) =>
    { encodeNullable :: forall a. f a -> Maybe a
    , decodeNullable :: forall a. Maybe a -> f a
    } -> NullableDef f

type RootDef :: OperationType -> (* -> *) -> * -> * -> *
data RootDef op m a r where
  QueryDef        :: (Response -> m r) -> Map Text (Resolver m a  ) -> RootDef QUERY        m a r
  MutationDef     :: (Response -> m r) -> Map Text (Resolver m a  ) -> RootDef MUTATION     m a r
  SubscriptionDef ::                      Map Text (Producer m a r) -> RootDef SUBSCRIPTION m a r
  UndefinedDef    ::                                                   RootDef op           m a r
