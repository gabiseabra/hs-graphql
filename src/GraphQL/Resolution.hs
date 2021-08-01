{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , GADTs
  , DuplicateRecordFields
  , NamedFieldPuns
  , RankNTypes
  , BlockArguments
  , ScopedTypeVariables
  , TypeApplications
  , ConstraintKinds
  , OverloadedStrings
#-}

module GraphQL.Resolution
  ( resolve
  ) where

import GraphQL.Class
import GraphQL.Selection
import GraphQL.IO.Output
import GraphQL.IO.Input
import GraphQL.Internal

import Control.Monad ((>=>), (<=<), join)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&), (<<<))
import qualified Data.Aeson as JSON
import Data.Bifunctor (first, second)
import Data.Bitraversable (bisequence)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable (Recursive(..), Corecursive(..), Base)
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Row (Rec)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec

data Step t m a where
  Step :: GraphQLOutputType m o => (a -> m o) -> [t] -> Step t m a
  Pure :: (a -> m JSON.Value) -> Step t m a

resolve :: forall a m t
  .  Monad m
  => IsSelection t
  => GraphQLOutputType m a
  => [t]
  -> V (a -> m JSON.Value)
resolve = fun
  where
    go :: forall f a. Step t m a -> V (a -> m JSON.Value)
    go (Step f s) = pure . (\g -> f >=> g) =<< fun s
    go (Pure f) = pure f
    fun :: forall a. GraphQLOutputType m a => [t] -> V (a -> m JSON.Value)
    fun s = sequenceResolver go =<< validate (typename $ Proxy @a) s (mkResolver (typeOf_ @a))

sequenceResolver :: forall a m f k
  .  Monad m
  => (forall a. f a -> V (a -> m JSON.Value))
  -> Resolver k f a
  -> V (a -> m JSON.Value)
sequenceResolver _ Leaf = pure $ pure . JSON.toJSON
-- TODO — if it's an branch wrapper, batch together all the same fields
sequenceResolver f (Wrap r) = pure . g =<< sequenceResolver f r
  where g f = pure . JSON.toJSON1 <=< traverse f
sequenceResolver f (Branch as) = pure . g =<< mapM f as
  where g as = pure . JSON.Object <=< sequence2 as
sequenceResolver f (Variant as)
  = fmap ((<<<) (fromMaybe (pure $ JSON.Object Map.empty)))
  $ foldr ((liftA2 . liftA2) (<|>) . g) (pure $ const Nothing) as
  where g (Case h a) = pure . (. h) . fmap =<< sequenceResolver f a

sequence2 :: (Traversable t, Monad m, Monad f) => t (f (m a)) -> f (m (t a))
sequence2 = fmap sequence . sequence

validate :: forall a k m t
  .  IsSelection t
  => Monad m
  => Typename
  -> [t]
  -> Resolver k (Field m) a
  -> V (Resolver k (Step t m) a)
validate _ [] Leaf = pure Leaf
validate t s (Wrap r) = pure . Wrap =<< validate t s r
validate t s@(_:_) (Branch as) = pure . Branch =<< validate'Object t s as
validate _ s@(_:_) (Variant as)
  = pure . Variant
  =<< (flip validate'Variant) as
  =<< groupSelection (Map.keys as) s
validate _ _ _ = Left "Invalid selection"

validate'Object :: forall a m t
  .  Monad m
  => IsSelection t
  => Typename
  -> [t]
  -> HashMap Text (Field m a)
  -> V (HashMap Text (Step t m a))
validate'Object t s as = pure . Map.fromList =<< mapM ((\s -> checkType s *> checkField s) . project) s
  where
    checkType = maybe (pure ()) (typenameMatches t) . typeConstraint . node
    checkField s@(NodeF (Sel { name = "__typename", alias }) _)
      = validate'__typename s
      *> pure
        ( fromMaybe "__typename" alias
        , Pure (const $ pure $ JSON.toJSON $ t)
        )
    checkField (NodeF (Sel { name, alias, input }) tail)
      = select t name as
      >>= apply tail input
      >>= \r -> pure (fromMaybe name alias, r)

validate'Variant :: forall a m t
  .  IsSelection t
  => Monad m
  => HashMap Typename [t]
  -> HashMap Typename (Case (Resolver BRANCH (Field m)) a)
  -> V (HashMap Typename (Case (Resolver BRANCH (Step t m)) a))
validate'Variant s = Map.traverseWithKey $ \k (Case f r) ->
  case Map.lookup k s of
    Nothing -> Case f <$> pure (Branch Map.empty)
    Just s' -> Case f <$> validate k s' r

validate'__typename :: TreeF Selection a -> V ()
validate'__typename (NodeF _ (_:_)) = Left "Invalid selection: __typename does not have fields"
validate'__typename (NodeF (Sel { input }) _)
  | Map.null input = Right ()
  | otherwise      = Left "Invalid selection: __typename does not have arguments"

typenameMatches :: Typename -> Typename -> V ()
typenameMatches t0 t1
  | t0 == t1  = Right ()
  | otherwise = Left $ "Typename mismatch: Expected " <> t0 <> ", got " <> t1

groupSelection :: forall t. IsSelection t => [Typename] -> [t] -> V (HashMap Typename [t])
groupSelection as = pure . Map.fromListWith (++) . join <=< mapM go
  where
    go :: t -> V [(Typename, [t])]
    go t =
      case ((typeConstraint &&& name) . node . project) t of
        (Nothing, "__typename") -> Right (fmap (\a -> (a, [t])) as)
        (Nothing, _) -> Left "No typename provided for union field"
        (Just a, _)
          | List.elem a as -> Right [(a, [t])]
          | otherwise      -> Left $ "Invalid typename " <> a <> " in union selection"

node :: TreeF a b -> a
node (NodeF a _) = a

select :: Typename -> Text -> HashMap Text a -> V a
select t k as = case Map.lookup k as of
  Nothing -> Left $ "Field " <> k <> " doesn't exist on type " <> t
  Just a -> Right a

apply :: [t] -> Input -> Field m a -> V (Step t m a)
apply s i (Field f) = (\i -> pure $ Step (\a -> f a i) s) =<< readInput i

type IsSelection t
  = ( Recursive t
    , Base t ~ TreeF Selection
    )
