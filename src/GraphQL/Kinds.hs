{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , GADTs
  , StandaloneKindSignatures
  , FlexibleContexts
  , FlexibleInstances
  , TypeApplications
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , UndecidableInstances
  , TypeOperators
  , InstanceSigs
  , OverloadedStrings
  , AllowAmbiguousTypes
#-}

module GraphQL.Kinds
  ( SCALAR
  , ENUM
  , OBJECT
  , INPUT_OBJECT
  , UNION
  , LIST
  , NULLABLE
  , type (.@)
  ) where

import qualified GHC.Generics as GR
import GHC.TypeLits (KnownSymbol, symbolVal)

import GraphQL.Internal
import GraphQL.Class
import GraphQL.IO.Input
import GraphQL.IO.Output

import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol)

import Control.Applicative (Alternative(..))
import Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Row (Rec, type (.!))
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import qualified Data.Row.Variants as Var
import Data.Text (Text)
import Data.Char (toUpper)
import Data.Functor.Compose (Compose(..))
import Data.Proxy (Proxy(..))

data SCALAR a where
  ScalarT ::
    ( JSON.FromJSON a
    , JSON.ToJSON a
    ) => SCALAR a

instance
  ( JSON.FromJSON a
  , JSON.ToJSON a
  ) => GraphQLTypeable SCALAR a where typeOf = ScalarT
instance GraphQLKind SCALAR where type KIND SCALAR = GQL_SCALAR
instance GraphQLInputKind SCALAR where
  readInputType ScalarT = liftJSONResult . JSON.fromJSON
instance GraphQLOutputKind m SCALAR where
  mkResolver ScalarT = Leaf JSON.toJSON

class GraphQLEnumG f where
  gql_fromEnum :: f a -> String
  gql_toEnum :: String -> Maybe (f a)
  gql_enumValues :: proxy f -> [String]

instance GraphQLEnumG cons => GraphQLEnumG (GR.D1 meta cons) where
  gql_fromEnum = gql_fromEnum . GR.unM1
  gql_toEnum = fmap GR.M1 . gql_toEnum @cons
  gql_enumValues _ = gql_enumValues (Proxy @cons)
instance (GraphQLEnumG l, GraphQLEnumG r) => GraphQLEnumG (l GR.:+: r) where
  gql_fromEnum (GR.L1 a) = gql_fromEnum a
  gql_fromEnum (GR.R1 a) = gql_fromEnum a
  gql_toEnum a = fmap GR.L1 (gql_toEnum a) <|> fmap GR.R1 (gql_toEnum a)
  gql_enumValues _ = gql_enumValues (Proxy @l) <> gql_enumValues (Proxy @r)
instance KnownSymbol sym => GraphQLEnumG (GR.C1 (GR.MetaCons sym f 'False) GR.U1) where
  gql_fromEnum _ = enumVal @sym
  gql_toEnum a
    | a == enumVal @sym = pure empty
    | otherwise         = Nothing
  gql_enumValues _ = [enumVal @sym]

enumVal :: forall sym. KnownSymbol sym => String
enumVal = fmap toUpper $ symbolVal $ Proxy @sym

data ENUM a where
  EnumT ::
    ( GR.Generic a
    , GraphQLEnumG (GR.Rep a)
    ) => ENUM a

instance
  ( GR.Generic a
  , GraphQLEnumG (GR.Rep a)
  ) => GraphQLTypeable ENUM a where typeOf = EnumT
instance GraphQLKind ENUM where type KIND ENUM = GQL_ENUM
instance GraphQLInputKind ENUM where
  readInputType EnumT
    = maybe (Left "Invalid enum value") (Right . GR.to)
    . gql_toEnum <=< liftJSONResult . JSON.fromJSON
instance GraphQLOutputKind m ENUM where
  mkResolver EnumT = Leaf (JSON.toJSON . gql_fromEnum . GR.from)

data OBJECT m a where
  ObjectT ::
    ( Rec.FromNative a
    , Row.AllUniqueLabels (Rec.NativeRow a)
    , Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
    , Row.FreeForall (Rec.NativeRow a)
    ) => OBJECT m a

instance
  ( Rec.FromNative a
  , Row.AllUniqueLabels (Rec.NativeRow a)
  , Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  , Row.FreeForall (Rec.NativeRow a)
  ) => GraphQLTypeable (OBJECT m) a where typeOf = ObjectT
instance GraphQLKind (OBJECT m) where type KIND (OBJECT m) = GQL_OBJECT
instance GraphQLOutputKind m (OBJECT m) where
  mkResolver :: forall a. OBJECT m a -> Resolver BRANCH (Field m) a
  mkResolver ObjectT
    = Branch
    $ Map.fromList
    $ eraseWithLabelsF
      @(GraphQLResolver m)
      @((->) a)
      @(Rec.NativeRow a)
      (Field . fmap applyInput)
    $ recordAccessors @a

data INPUT_OBJECT a where
  InputObjectT ::
    ( Rec.ToNative a
    , Row.AllUniqueLabels (Rec.NativeRow a)
    , Row.Forall (Rec.NativeRow a) GraphQLInputType
    , Row.FreeForall (Rec.NativeRow a)
    ) => INPUT_OBJECT a

instance
  ( Rec.ToNative a
  , Row.AllUniqueLabels (Rec.NativeRow a)
  , Row.Forall (Rec.NativeRow a) GraphQLInputType
  , Row.FreeForall (Rec.NativeRow a)
  ) => GraphQLTypeable INPUT_OBJECT a where typeOf = InputObjectT
instance GraphQLKind INPUT_OBJECT where type KIND INPUT_OBJECT = GQL_INPUT_OBJECT
instance GraphQLInputKind INPUT_OBJECT where readInputType InputObjectT = pure . Rec.toNative <=< readInputFields

data UNION m a where
  UnionT ::
    ( Var.FromNative a
    , Row.AllUniqueLabels (Var.NativeRow a)
    , Row.Forall (Var.NativeRow a) (GraphQLObjectType m)
    ) => UNION m a

instance
  ( Var.FromNative a
  , Row.AllUniqueLabels (Var.NativeRow a)
  , Row.Forall (Var.NativeRow a) (GraphQLObjectType m)
  ) => GraphQLTypeable (UNION m) a where typeOf = UnionT
instance GraphQLKind (UNION m) where type KIND (UNION m) = GQL_UNION
instance GraphQLOutputKind m (UNION m) where
  mkResolver :: forall a. UNION m a -> Resolver VARIANT (Field m) a
  mkResolver UnionT
    = Variant
    $ Map.fromList
    $ eraseF
      @(GraphQLObjectType m)
      @(Compose ((->) a) Maybe)
      @(Var.NativeRow a)
      mkCase
    $ variantCases @(GraphQLObjectType m) @a
    where
      mkCase :: forall b. GraphQLObjectType m b => Compose ((->) a) Maybe b -> (Text, Case (Resolver BRANCH (Field m)) a)
      mkCase f = (typename (Proxy @b), Case (getCompose f) (mkResolver (typeOf_ @b)))

data LIST t a where
  ListT ::
    ( Functor f
    , Traversable f
    , Applicative f
    , Monoid (f a)
    , JSON.ToJSON1 f
    ) => t a
      -> LIST t (f a)

instance
  ( Functor f
  , Traversable f
  , Applicative f
  , Monoid (f a)
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (LIST t) (f a) where typeOf = ListT typeOf
instance GraphQLKind t => GraphQLKind (LIST t) where type KIND (LIST t) = GQL_LIST (KIND t)
instance GraphQLInputKind t => GraphQLInputKind (LIST t) where
  readInputType (ListT t) (JSON.Array array) = pure . foldMap pure =<< traverse (readInputType t) array
  readInputType _ _ = Left "Expected an array"
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (LIST t) where
  mkResolver (ListT t) = Wrap (mkResolver @m @t t)

data NULLABLE t a where
  NullableT ::
    ( Functor f
    , Traversable f
    , Alternative f
    , JSON.ToJSON1 f
    ) => t a
      -> NULLABLE t (f a)

instance
  ( Functor f
  , Traversable f
  , Alternative f
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (NULLABLE t) (f a) where typeOf = NullableT typeOf
instance GraphQLKind t => GraphQLKind (NULLABLE t) where type KIND (NULLABLE t) = GQL_NULLABLE (KIND t)
instance GraphQLInputKind t => GraphQLInputKind (NULLABLE t) where
  readInputType (NullableT _) JSON.Null = pure empty
  readInputType (NullableT t) v = fmap pure (readInputType t v)
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (NULLABLE t) where
  mkResolver (NullableT t) = Wrap (mkResolver @m @t t)

data VAR k r a where
  VarT ::
    ( Row.HasType k t r
    , GraphQLKind t
    ) => t a
      -> VAR k r a

type (.@) :: Symbol -> Row.Row (* -> *) -> (* -> *)
type (.@) k r = VAR k r

instance
  ( Row.HasType k t r
  , GraphQLKind t
  , GraphQLTypeable t a
  ) => GraphQLTypeable (VAR k r) a where typeOf = VarT (typeOf @t @a)
instance
  ( Row.HasType k t r
  , GraphQLKind t
  ) => GraphQLKind (VAR k r) where type KIND (VAR k r) = KIND (r .! k)
instance
  ( Row.HasType k t r
  , GraphQLInputKind t
  ) => GraphQLInputKind (VAR k r) where readInputType (VarT t) = readInputType t
instance
  ( Row.HasType k t r
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (VAR k r) where mkResolver (VarT t) = mkResolver t
