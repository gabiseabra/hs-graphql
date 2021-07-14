{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.Internal where

import Data.Functor.Const (Const(..))
import Data.Proxy (Proxy(..))
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec

mapRow :: forall c r b
  .  Row.Forall r c
  => Row.AllUniqueLabels r
  => (forall proxy l a. (Row.KnownSymbol l, c a) => Row.Label l -> proxy a -> b)
  -> [b]
mapRow f = getConst $ Rec.fromLabelsA @c @(Const [b]) @r g
  where
    g :: forall l a. (Row.KnownSymbol l, c a) => Row.Label l -> Const [b] a
    g l = Const [f l (Proxy @a)]
