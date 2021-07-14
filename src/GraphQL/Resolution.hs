{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.Resolution where

import GraphQL.Class
import GraphQL.Selection
import GraphQL.IO.Output
import GraphQL.IO.Input

import Control.Monad ((<=<), liftM, liftM2)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor(..), bimap)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable (Recursive (..), Corecursive (..), Base)

data ResolutionF a r where
  Res :: Resolution n a r -> ResolutionF a r

instance Functor (ResolutionF a) where fmap f (Res r) = Res (fmap f r)
instance Foldable (ResolutionF a) where foldMap f (Res r) = foldMap f r
instance Traversable (ResolutionF a) where traverse f (Res r) = fmap Res (traverse f r)
instance Bifunctor ResolutionF where bimap f g (Res r) = Res (bimap f g r)
instance ToJSON a => ToJSON (ResolutionF a a) where toJSON (Res r) = JSON.toJSON r

select
  :: Recursive t
  => Base t ~ SelectionTreeF
  => [Base t t]
  -> Resolution n a r
  -> Resolution n a (r, Base t t)
select s (Leaf a) = Leaf a
select s (Wrapper r) = Wrapper (fmap (select s) r)
select s (Branch r) = Branch (f =<< s)
  where
    f sel@(NodeF (Sel { name, alias }) s') =
      case List.lookup name r of
        Nothing -> []
        Just v -> [(fromMaybe name alias, (v, sel))]

anaM
  :: Monad m
  => Traversable (Base t)
  => Corecursive t
  => (a -> m (Base t a))
  -> a -> m t
anaM psi = let h = return . embed <=< mapM h <=< psi in h

unfoldResolution :: forall u t m a
  .  Monad m
  => Recursive t
  => Base t ~ SelectionTreeF
  => Corecursive u
  => Base u ~ ResolutionF JSON.Value
  => GraphQLType a
  => GraphQLOutputKind m (KindOf a)
  => a
  -> [t]
  -> m u
unfoldResolution a0 = anaM coalg . root
  where
    root t = (rootResolver a0, rootSelection t)
    coalg
      :: Monad m
      => Recursive t
      => Base t ~ SelectionTreeF
      => (Resolver m, SelectionTreeF t)
      -> m (ResolutionF JSON.Value (Resolver m, SelectionTreeF t))
    coalg (Resolver f, NodeF _ sel)
      = return
      . Res
      . select (fmap project sel)
      . resolve typeOf
      =<< f ()

foldResolution :: Recursive u => Base u ~ ResolutionF JSON.Value => u -> JSON.Value
foldResolution = cata JSON.toJSON
