{-# LANGUAGE
    TypeApplications
  , ScopedTypeVariables
  , AllowAmbiguousTypes
  , ConstraintKinds
  , RankNTypes
  , FlexibleContexts
#-}

module GraphQL.Internal where

import qualified Data.Aeson as JSON
import Data.Functor.Const (Const(..))
import Data.Proxy (Proxy(..))
import qualified Data.Row as Row
import Data.Row.Records (Rec)
import qualified Data.Row.Records as Rec
import Data.String (IsString)

mapRow :: forall c r b
  .  Row.Forall r c
  => Row.AllUniqueLabels r
  => (forall proxy l a. (Row.KnownSymbol l, c a) => Row.Label l -> proxy a -> b)
  -> [b]
mapRow f = getConst $ Rec.fromLabelsA @c @(Const [b]) @r g
  where
    g :: forall l a. (Row.KnownSymbol l, c a) => Row.Label l -> Const [b] a
    g l = Const [f l (Proxy @a)]

accessors :: forall a
  .  Rec.FromNative a
  => Row.FreeForall (Rec.NativeRow a)
  => Rec (Rec.Map ((->) a) (Rec.NativeRow a))
accessors = Rec.distribute (Rec.fromNative @a)

eraseWithLabelsF :: forall c f r k b
  .  IsString k
  => Row.Forall r c
  => (forall a. c a => f a -> b)
  -> Rec (Rec.Map f r)
  -> [(k, b)]
eraseWithLabelsF f
  = zip (Rec.labels @r @c)
  . getConst
  . Rec.sequence' @(Const [b]) @r @c
  . Rec.transform @c @r @f @(Const [b]) (Const . pure . f)

liftJSONResult :: MonadFail m => JSON.Result a -> m a
liftJSONResult (JSON.Error e) = fail e
liftJSONResult (JSON.Success a) = pure a
