{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , TypeOperators
  , AllowAmbiguousTypes
  , StandaloneKindSignatures
  , TypeApplications
  , ScopedTypeVariables
  , FlexibleInstances
  , FlexibleContexts
  , PolyKinds
  , RankNTypes
  , ConstraintKinds
  , MultiParamTypeClasses
#-}

module GraphQL.TypeSystem.TypeDefs where

import GraphQL.Internal
import GraphQL.TypeSystem.Main

import GHC.Exts (Constraint)
import qualified GHC.Generics as GR
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Control.Applicative (Alternative(..))
import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty(..))
import Data.Row (Row, Rec, Var)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import qualified Data.Row.Variants as Var
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))
import Data.Functor.Compose (Compose(..))
import Data.Coerce (coerce)

scalarDef :: forall a
  .  JSON.FromJSON a
  => JSON.ToJSON a
  => Typename
  -> TypeDef SCALAR a
scalarDef ty
  = ScalarType ty Nothing $ ScalarDef JSON.toJSON (maybeJSON . JSON.fromJSON)
  where
    maybeJSON (JSON.Error _) = Nothing
    maybeJSON (JSON.Success a) = Just a

enumDef :: forall a
  .  GR.Generic a
  => GraphQLEnumG (GR.Rep a)
  => Typename
  -> TypeDef ENUM a
enumDef ty
  = EnumType ty Nothing
  $ EnumDef (_enumValues $ Proxy @(GR.Rep a))
              (_encodeEnum . GR.from)
              (fmap GR.to . _decodeEnum)

type GraphQLEnumG :: (* -> *) -> Constraint
class GraphQLEnumG f where
  _encodeEnum :: f a -> Text
  _decodeEnum :: Text -> Maybe (f a)
  _enumValues :: proxy f -> NonEmpty EnumValueDef

instance GraphQLEnumG cons => GraphQLEnumG (GR.D1 meta cons) where
  _encodeEnum = _encodeEnum . GR.unM1
  _decodeEnum = fmap GR.M1 . _decodeEnum @cons
  _enumValues _ = _enumValues (Proxy @cons)
instance (GraphQLEnumG l, GraphQLEnumG r) => GraphQLEnumG (l GR.:+: r) where
  _encodeEnum (GR.L1 a) = _encodeEnum a
  _encodeEnum (GR.R1 a) = _encodeEnum a
  _decodeEnum a = fmap GR.L1 (_decodeEnum a) <|> fmap GR.R1 (_decodeEnum a)
  _enumValues _ = _enumValues (Proxy @l) <> _enumValues (Proxy @r)
instance KnownSymbol sym => GraphQLEnumG (GR.C1 (GR.MetaCons sym f 'False) GR.U1) where
  _encodeEnum _ = value $ _enumValue @sym
  _decodeEnum a
    | a == value (_enumValue @sym) = pure empty
    | otherwise                    = Nothing
  _enumValues _ = _enumValue @sym :| []

_enumValue :: forall sym. KnownSymbol sym => EnumValueDef
_enumValue = EnumValueDef value Nothing
  where value = Text.toUpper $ Text.pack $ symbolVal $ Proxy @sym

inputObjectDef :: forall a
  .  Rec.ToNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) GraphQLInputType
  => Typename
  -> TypeDef INPUT_OBJECT a
inputObjectDef ty
  = InputObjectType ty Nothing $ InputObjectDef Rec.toNative

unionDef :: forall a m
  .  Var.FromNative a
  => Row.AllUniqueLabels (Var.NativeRow a)
  => Row.Forall (Var.NativeRow a) (GraphQLObjectType m)
  => Typename
  -> TypeDef (UNION @m) a
unionDef ty
  = UnionType ty Nothing
  $ UnionDef
  $ Map.fromList
  $ eraseF
    @(GraphQLObjectType m) @(Compose ((->) a) Maybe) @(Var.NativeRow a)
    mkCase
  $ variantCases @(GraphQLObjectType m) @a
  where
    mkCase :: forall b. GraphQLObjectType m b => Compose ((->) a) Maybe b -> (Text, Case m a)
    mkCase f = (typename $ typeDef @b, Case $ getCompose f)

-- * Object types

objectDef :: forall a m
  .  Applicative m
  => Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) (GraphQLOutputType m)
  => Row.FreeForall (Rec.NativeRow a)
  => Typename
  -> TypeDef (OBJECT @m) a
objectDef
  = mkObjectDef @(GraphQLOutputType m)
  $ Some . FieldDef Nothing . const @_ @() . fmap pure

resolverDef :: forall a m
  .  Applicative m
  => Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  => Row.FreeForall (Rec.NativeRow a)
  => Typename
  -> TypeDef (OBJECT @m) a
resolverDef = mkObjectDef @(GraphQLResolver m) $ Some . resolverFieldDef

canonicalDef :: forall sym m a proxy
  .  GetFields sym m a
  => Typename
  -> proxy sym
  -> TypeDef (OBJECT @m) a
canonicalDef ty _
  = ObjectType ty Nothing
  $ ObjectDef
  $ Map.fromList
  $ getFields @sym @m @a

class GraphQLResolver m a where
  type ResolverOutput a :: *
  resolverFieldDef :: (ctx -> a) -> FieldDef m ctx (ResolverOutput a)

instance
  ( GraphQLInput i
  , GraphQLOutputType m a
  , Applicative m
  ) => GraphQLResolver m (i -> m a) where
  type ResolverOutput (i -> m a) = a
  resolverFieldDef = FieldDef Nothing . flip

type GraphQLField :: (* -> *) -> * -> Symbol -> Constraint
class
  ( Applicative m
  , GraphQLOutputType m (OutputOf a sym)
  ) => GraphQLField m a sym where
  type OutputOf a sym :: *
  fieldDef :: FieldDef m a (OutputOf a sym)

type GetFields :: [Symbol] -> (* -> *) -> * -> Constraint
class GetFields sym m a where getFields :: [(Text, FieldAp m a)]
instance GetFields '[] m a where getFields = []
instance
  ( GraphQLField m a sym
  , GetFields tail m a
  , KnownSymbol sym
  ) => GetFields (sym ': tail) m a where
    getFields
      = ( Text.pack $ symbolVal $ Proxy @sym
        , Some $ fieldDef @m @a @sym
        ) : getFields @tail @m @a

mkObjectDef :: forall c a m
  .  Applicative m
  => Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) c
  => Row.FreeForall (Rec.NativeRow a)
  => (forall b. c b => (a -> b) -> FieldAp m a)
  -> Typename
  -> TypeDef (OBJECT @m) a
mkObjectDef fn ty
  = ObjectType ty Nothing
  $ ObjectDef
  $ Map.fromList
  $ eraseWithLabelsF @c @((->) a) @(Rec.NativeRow a) fn
  $ recordAccessors @a
