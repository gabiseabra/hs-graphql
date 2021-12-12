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
import           Control.Arrow (Kleisli(..))
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
enumDef ty = EnumType ty Nothing $ Map.fromList ((fmap . fmap . fmap) GR.to . mkEnumValues $ Proxy @(GR.Rep a))

type GraphQLEnumG :: (* -> *) -> Constraint
class GraphQLEnumG f where
  mkEnumValues :: proxy f -> [(Text, EnumValue (f a))]

instance GraphQLEnumG cons => GraphQLEnumG (GR.D1 meta cons) where
  mkEnumValues _ = (fmap . fmap) GR.M1 <$> mkEnumValues (Proxy @cons)
instance (GraphQLEnumG l, GraphQLEnumG r) => GraphQLEnumG (l GR.:+: r) where
  mkEnumValues _ = (fmap . fmap . fmap) GR.L1 (mkEnumValues (Proxy @l)) <> (fmap . fmap . fmap) GR.R1 (mkEnumValues (Proxy @r))
instance KnownSymbol sym => GraphQLEnumG (GR.C1 (GR.MetaCons sym f 'False) GR.U1) where
  mkEnumValues a = pure (lbl, EnumValue mempty empty)
    where lbl = Text.toUpper $ Text.pack $ symbolVal $ Proxy @sym

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
unionDef ty = UnionType ty Nothing Var.fromNative

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
    go = Exists2 @GraphQLInput @(ResolverInput r)
                 @(GraphQLOutputType m) @(ResolverOutput r)
      . Field mempty . fmap Kleisli . flip . fmap mkFieldResolver

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
