{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module GraphQL.Internal
  ( cataM
  , mapRow
  , recordAccessors
  , variantCases
  , eraseWithLabelsF
  , eraseF
  , hoistCofreeM
  , Exists(..)
  , Exists1(..)
  , Exists2(..)
  ) where

import           Control.Arrow                  ( Kleisli(..) )
import           Control.Comonad.Cofree         ( Cofree(..) )
import           Control.Monad                  ( (<=<) )
import qualified Data.Aeson                    as JSON
import           Data.Functor.Const             ( Const(..) )
import           Data.Functor.Foldable          ( Base
                                                , Recursive(..)
                                                )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Row                      as Row
import           Data.Row                       ( type (.-)
                                                , Rec
                                                , Var
                                                )
import           Data.Row.Dictionaries          ( (\\)
                                                , mapExtendSwap
                                                )
import qualified Data.Row.Internal             as Row
                                                ( metamorph )
import qualified Data.Row.Records              as Rec
import qualified Data.Row.Variants             as Var
import           Data.String                    ( IsString )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )

cataM
  :: Monad m
  => Traversable (Base t)
  => Recursive t
  => (Base t a -> m a) -- ^ algebra
  -> t
  -> m a
cataM phi = h where h = phi <=< mapM h . project

mapRow
  :: forall c r b
   . Row.Forall r c
  => Row.AllUniqueLabels r
  => (  forall proxy l a
      . (Row.KnownSymbol l, c a)
     => Row.Label l
     -> proxy a
     -> b
     )
  -> [b]
mapRow f = getConst $ Rec.fromLabelsA @c @(Const [b]) @r g
 where
  g :: forall l a . (Row.KnownSymbol l, c a) => Row.Label l -> Const [b] a
  g l = Const [f l (Proxy @a)]

recordAccessors
  :: forall a
   . Rec.FromNative a
  => Row.FreeForall (Rec.NativeRow a)
  => Rec (Rec.Map ((->) a) (Rec.NativeRow a))
recordAccessors = Rec.distribute (Rec.fromNative @a)

newtype VC0 a r = VC0 { unVC0 :: a -> Maybe (Var r) }
newtype VC1 a r = VC1 { unVC1 :: Rec (Rec.Map (Kleisli Maybe a) r) }

variantCases
  :: forall c a
   . Var.FromNative a
  => Var.AllUniqueLabels (Var.NativeRow a)
  => Row.Forall (Var.NativeRow a) c
  => Rec (Rec.Map (Kleisli Maybe a) (Var.NativeRow a))
variantCases =
  unVC1
    $ Row.metamorph @_ @(Var.NativeRow a) @c @(,) @(VC0 a) @(VC1 a)
        @(Kleisli Maybe a)
        Proxy
        empty
        uncons
        cons
    $ VC0 (Just . Var.fromNative)
 where
  empty _ = VC1 Rec.empty
  uncons l (VC0 f) = (VC0 $ restrict l <=< f, Kleisli $ Var.view l <=< f)
  cons
    :: forall l b r
     . Row.KnownSymbol l
    => Row.Label l
    -> (VC1 a r, Kleisli Maybe a b)
    -> VC1 a (Rec.Extend l b r)
  cons l (VC1 r, c) =
    VC1 $ Rec.extend l c r \\ mapExtendSwap @(Kleisli Maybe a) @l @b @r

restrict
  :: forall r l
   . Row.KnownSymbol l
  => Row.Label l
  -> Var r
  -> Maybe (Var (r .- l))
restrict l v = case Var.trial v l of
  Left  v' -> Just v'
  Right _  -> Nothing

eraseWithLabelsF
  :: forall c f r b k
   . IsString k
  => Row.Forall r c
  => (forall a . c a => f a -> b)
  -> Rec (Rec.Map f r)
  -> [(k, b)]
eraseWithLabelsF f = zip (Rec.labels @r @c) . eraseF @c @f @r @b f

eraseF
  :: forall c f r b
   . Row.Forall r c
  => (forall a . c a => f a -> b)
  -> Rec (Rec.Map f r)
  -> [b]
eraseF f =
  getConst
    . Rec.sequence' @(Const [b]) @r @c
    . Rec.transform @c @r @f @(Const [b]) (Const . pure . f)

hoistCofreeM
  :: (Traversable f, Monad m)
  => (forall x . f x -> m (g x))
  -> Cofree f a
  -> m (Cofree g a)
hoistCofreeM f (x :< y) = (x :<) <$> (f =<< traverse (hoistCofreeM f) y)

data Exists c where Exists ::c a => a -> Exists c

data Exists1 f c where Exists1 ::c a => f a -> Exists1 f c

data Exists2 p c1 c2 where Exists2 ::(c1 a, c2 b) => p a b -> Exists2 p c1 c2
