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
import GraphQL.Response (Response)
import GraphQL.TypeSystem.Main

import GHC.Exts (Constraint)
import qualified GHC.Generics as GR
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Control.Applicative (Alternative(..))
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
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
import Data.Profunctor.Cayley (Cayley(..))
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
objectDef ty
  = ObjectType ty Nothing
  $ ObjectDef
  $ mkFieldMap @(GraphQLOutputType m)
  $ Some2 . FieldDef Nothing . const @_ @() . Compose . fmap pure

resolverDef :: forall a m
  .  Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  => Row.FreeForall (Rec.NativeRow a)
  => Typename
  -> TypeDef (OBJECT @m) a
resolverDef ty
  = ObjectType ty Nothing
  $ ObjectDef
  $ mkFieldMap @(GraphQLResolver m)
  $ Some2 . resolverFieldDef

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

queryDef :: forall m a r
  .  Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  => Row.FreeForall (Rec.NativeRow a)
  => Typename
  -> (Response -> m r)
  -> TypeDef (ROOT @QUERY @m @r) a
queryDef ty f
  = RootType ty
  $ QueryDef f
  $ mkFieldMap @(GraphQLResolver m)
  $ Some2 . resolverFieldDef

mutationDef :: forall m a r
  .  Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  => Row.FreeForall (Rec.NativeRow a)
  => Typename
  -> (Response -> m r)
  -> TypeDef (ROOT @MUTATION @m @r) a
mutationDef ty f
  = RootType ty
  $ MutationDef f
  $ mkFieldMap @(GraphQLResolver m)
  $ Some2 . resolverFieldDef

class GraphQLResolver m a where
  type ResolverInput a :: *
  type ResolverOutput a :: *
  resolverFieldDef :: (ctx -> a) -> ResolverF m ctx (ResolverInput a) (ResolverOutput a)
-- (ctx -> m )

instance
  ( GraphQLInput i
  , GraphQLOutputType m a
  ) => GraphQLResolver m (i -> m a) where
  type ResolverInput (i -> m a) = i
  type ResolverOutput (i -> m a) = a
  resolverFieldDef = mkResolver Nothing . flip

type GraphQLField :: (* -> *) -> * -> Symbol -> Constraint
class
  ( GraphQLOutputType m (OutputOf a sym)
  , GraphQLInput (InputOf a sym)
  ) => GraphQLField m a sym where
  type OutputOf a sym :: *
  type InputOf a sym :: *
  type InputOf a sym = ()
  fieldDef ::  ResolverF m a (InputOf a sym) (OutputOf a sym)
  fieldDef = mkResolver (description @m @a @sym) (resolver @m @a @sym)
  description :: Maybe Text
  description = Nothing
  resolver :: (InputOf a sym) -> a -> m (OutputOf a sym)

mkResolver :: (GraphQLInput i, GraphQLOutputType m o) => Maybe Text -> (i -> a -> m o) -> ResolverF m a i o
mkResolver desc = FieldDef desc . (Compose .)

type GetFields :: [Symbol] -> (* -> *) -> * -> Constraint
class GetFields sym m a where getFields :: [(Text, Resolver m a)]
instance GetFields '[] m a where getFields = []
instance
  ( GraphQLField m a sym
  , GetFields tail m a
  , KnownSymbol sym
  ) => GetFields (sym ': tail) m a where
    getFields
      = ( Text.pack $ symbolVal $ Proxy @sym
        , Some2 $ fieldDef @m @a @sym
        ) : getFields @tail @m @a
        where
          desc = description @m @a @sym
          res = Compose . resolver @m @a @sym

class GraphQLProducer m r a where
  type ProducerInput a :: *
  type ProducerOutput a :: *
  producerFieldDef :: (ctx -> a) -> ProducerF m ctx r (ProducerInput a) (ProducerOutput a)

instance
  ( GraphQLInput i
  , GraphQLOutputType m a
  ) => GraphQLProducer m r (i -> (a -> m Response) -> m r) where
  type ProducerInput (i -> (a -> m Response) -> m r) = i
  type ProducerOutput (i -> (a -> m Response) -> m r) = a
  producerFieldDef = FieldDef Nothing . (Producer .) . flip

mkFieldMap :: forall c a m
  .  Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) c
  => Row.FreeForall (Rec.NativeRow a)
  => (forall b. c b => (a -> b) -> Resolver m a)
  -> Map Text (Resolver m a)
mkFieldMap fn
  = Map.fromList
  $ eraseWithLabelsF @c @((->) a) @(Rec.NativeRow a) fn
  $ recordAccessors @a
