{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module GraphQL.TypeSystem.TypeDefs where

import           Control.Applicative (Alternative(..))
import           Control.Arrow (Kleisli(..), (&&&))
import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy(..))
import qualified Data.Row as Row
import           Data.Row (Row, Rec, Var)
import qualified Data.Row.Records as Rec
import qualified Data.Row.Variants as Var
import qualified Data.Text as Text
import           Data.Text (Text)
import           GHC.Exts (Constraint)
import qualified GHC.Generics as GR
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           GraphQL.Internal
import           GraphQL.Response (Response)
import           GraphQL.TypeSystem.Main
import Control.Lens (view)

scalarDef :: forall a
  .  JSON.FromJSON a
  => JSON.ToJSON a
  => Typename
  -> TypeDef SCALAR a
scalarDef ty = ScalarType ty Nothing

enumDef :: forall a
  .  GR.Generic a
  => GraphQLEnumG (GR.Rep a)
  => Typename
  -> TypeDef ENUM a
enumDef ty = EnumType ty Nothing enumValues encodeEnum
  where
    enumValues = Map.fromList . (fmap . fmap . fmap) GR.to . enumValues' $ Proxy @(GR.Rep a)
    encodeEnum = encodeEnum' . GR.from

type GraphQLEnumG :: (* -> *) -> Constraint
class GraphQLEnumG f where
  enumValues' :: proxy f -> [(Text, EnumValue (f a))]
  encodeEnum' :: f a -> Text

instance GraphQLEnumG cons => GraphQLEnumG (GR.D1 meta cons) where
  enumValues' _ = (fmap . fmap) GR.M1 <$> enumValues' (Proxy @cons)
  encodeEnum' = encodeEnum' . GR.unM1
instance (GraphQLEnumG l, GraphQLEnumG r) => GraphQLEnumG (l GR.:+: r) where
  enumValues' _ = (fmap . fmap . fmap) GR.L1 (enumValues' (Proxy @l)) <> (fmap . fmap . fmap) GR.R1 (enumValues' (Proxy @r))
  encodeEnum' (GR.R1 a) = encodeEnum' a
  encodeEnum' (GR.L1 a) = encodeEnum' a
instance KnownSymbol sym => GraphQLEnumG (GR.C1 (GR.MetaCons sym f 'False) GR.U1) where
  enumValues' a = pure (enumLabel @sym, EnumValue mempty empty)
  encodeEnum' _ = enumLabel @sym

enumLabel :: forall sym. KnownSymbol sym => Text
enumLabel = Text.toUpper . Text.pack . symbolVal $ Proxy @sym

inputObjectDef :: forall a
  .  Rec.ToNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) GraphQLInputType
  => Typename
  -> TypeDef INPUT_OBJECT a
inputObjectDef ty = InputObjectType ty Nothing Rec.toNative

unionDef :: forall a m
  .  Var.FromNative a
  => Row.AllUniqueLabels (Var.NativeRow a)
  => Row.Forall (Var.NativeRow a) (GraphQLObjectType m)
  => Typename
  -> TypeDef (UNION @m) a
unionDef ty
  = UnionType ty Nothing
  $ Map.fromList
  $ eraseF
    @(GraphQLObjectType m) @(Kleisli Maybe a) @(Var.NativeRow a)
    go
  $ variantCases @(GraphQLObjectType m) @a
  where
    go :: forall b. GraphQLObjectType m b => Kleisli Maybe a b -> (Text, Variant m a)
    go = const (view _typename (typeDef @b)) &&& Exists1

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
  $ Map.fromList
  $ eraseWithLabelsF @(GraphQLOutputType m) @((->) a) @(Rec.NativeRow a) go
  $ recordAccessors @a
  where
    go :: forall r. GraphQLOutputType m r => (a -> r) -> Resolver m a
    go = Exists2 @GraphQLInput @()
                 @(GraphQLOutputType m) @r
      . Field mempty . const . Kleisli . (pure .)

resolverDef :: forall a m
  .  Rec.FromNative a
  => Row.AllUniqueLabels (Rec.NativeRow a)
  => Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  => Row.FreeForall (Rec.NativeRow a)
  => Typename
  -> TypeDef (OBJECT @m) a
resolverDef ty
  = ObjectType ty Nothing
  $ Map.fromList
  $ eraseWithLabelsF @(GraphQLResolver m) @((->) a) @(Rec.NativeRow a) go
  $ recordAccessors @a
  where
    go :: forall r. GraphQLResolver m r => (a -> r) -> Resolver m a
    go = resolver mempty . fmap mkFieldResolver

class
  ( GraphQLInput (ResolverInput a)
  , GraphQLOutputType m (ResolverOutput a)
  ) => GraphQLResolver m a where
  type ResolverInput a :: *
  type ResolverOutput a :: *
  mkFieldResolver :: a -> ResolverInput a -> m (ResolverOutput a)

instance
  ( GraphQLInput i
  , GraphQLOutputType m a
  ) => GraphQLResolver m (i -> m a) where
  type ResolverInput (i -> m a) = i
  type ResolverOutput (i -> m a) = a
  mkFieldResolver = id

resolver
  :: GraphQLOutputType m r
  => GraphQLInput i
  => Maybe Text
  -> (a -> i -> m r)
  -> Resolver m a
resolver desc = Exists2 . Field desc . fmap Kleisli . flip
