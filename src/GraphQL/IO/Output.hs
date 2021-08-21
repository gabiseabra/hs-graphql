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
  , PolyKinds
  , TypeOperators
#-}

module GraphQL.IO.Output
  ( resolve
  , STree
  ) where

import GraphQL.TypeSystem
import GraphQL.Types
import GraphQL.Selection
import GraphQL.IO.Input
import GraphQL.Internal

import Control.Monad ((>=>), (<=<), join)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow (Kleisli(..), (&&&), (<<<))
import qualified Data.Aeson as JSON
import Data.Bifunctor (first, second)
import Data.Bitraversable (bisequence)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable (Recursive(..), Corecursive(..), Base)
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Row (Rec)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import Data.Fix (Fix)
import Data.Functor.Compose (Compose(..))

type STree = Fix (TreeF Selection)

data Exists c f where Exists :: c a => f a -> Exists c f

data NodeType = LEAF | BRANCH | VARIANT | WRAPPER NodeType

type Step m a = Exists (GraphQLOutputType m) (Kleisli m a)

type Branch m a = HashMap Text ([STree], Step m a)

type Variant m a = Map Typename (VariantCase m a)

data VariantCase m a where VariantCase :: (a -> Maybe b) -> Resolution BRANCH m b -> VariantCase m a

type family NodeTypeOf k where
  NodeTypeOf SCALAR       = LEAF
  NodeTypeOf ENUM         = LEAF
  NodeTypeOf OBJECT       = BRANCH
  NodeTypeOf UNION        = VARIANT
  NodeTypeOf (LIST k)     = WRAPPER (NodeTypeOf k)
  NodeTypeOf (NULLABLE k) = WRAPPER (NodeTypeOf k)

data Resolution t m a where
  Leaf :: (a -> m JSON.Value) -> Resolution LEAF m a
  Branch :: Branch m a -> Resolution BRANCH m a
  Variant :: Variant m a -> Resolution VARIANT m a
  Wrapper ::
    ( Traversable f
    ) => (f JSON.Value -> JSON.Value)
      -> Resolution t m a
      -> Resolution (WRAPPER t) m (f a)

resolve :: forall a m
  .  Monad m
  => GraphQLOutputType m a
  => [STree]
  -> V (a -> m JSON.Value)
resolve = fun
  where
    go :: forall a. ([STree], Step m a) -> V (a -> m JSON.Value)
    go (t, Exists f) = pure . (\g -> runKleisli f >=> g) =<< fun t
    fun :: forall a. GraphQLOutputType m a => [STree] -> V (a -> m JSON.Value)
    fun t = let def = typeDef @a in sequenceResolver go =<< validate t def

sequenceResolver :: forall a m f k
  .  Monad m
  => (forall a. ([STree], Step m a) -> V (a -> m JSON.Value))
  -> Resolution k m a
  -> V (a -> m JSON.Value)
sequenceResolver _ (Leaf f) = pure f
-- TODO — if it's an branch wrapper, batch together all the same fields
sequenceResolver f (Wrapper h r) = pure . g =<< sequenceResolver f r
  where g f = pure . h <=< traverse f
sequenceResolver f (Branch as) = pure . g =<< mapM f as
  where g as = pure . JSON.Object <=< sequence2 as
sequenceResolver f (Variant as)
  = fmap ((<<<) (fromMaybe $ error "Unexpected return type from union type"))
  $ foldr ((liftA2 . liftA2) (<|>) . g) (pure $ const Nothing) as
  where g (VariantCase h a) = pure . (. h) . fmap =<< sequenceResolver f a

sequence2 :: (Traversable t, Monad m, Monad f) => t (f (m a)) -> f (m (t a))
sequence2 = fmap sequence . sequence

validate :: forall k m a
  .  Monad m
  => k !! m
  => [STree]
  -> TypeDef k a
  -> V (Resolution (NodeTypeOf k) m a)
validate []      (ScalarType   _  _ def                ) = pure (Leaf $ pure . encodeScalar def)
validate []      (EnumType     _  _ def                ) = pure (Leaf $ pure . JSON.toJSON . encodeEnum def)
validate t       (ListType     _  k (ListDef     enc _)) = fmap (Wrapper $ JSON.toJSON1 . enc) $ validate t k
validate t       (NullableType _  k (NullableDef enc _)) = fmap (Wrapper $ JSON.toJSON1 . enc) $ validate t k
validate t@(_:_) (ObjectType   ty _ def                ) = fmap Branch $ validate'Object t ty def
validate t@(_:_) (UnionType    ty _ def                ) = fmap Variant $ validate'Union t ty def
validate _       (ScalarType   ty _ _                  ) = Left $ "Scalar type " <> ty <> " cannot have a selection"
validate _       (EnumType     ty _ _                  ) = Left $ "Enum type " <> ty <> " cannot have a selection"
validate _       (ObjectType   ty _ _                  ) = Left $ "Object type " <> ty <> " must have a selection"
validate _       (UnionType    ty _ _                  ) = Left $ "Union type " <> ty <> " must have a selection"

validate'Object :: forall m m0 a
  .  Monad m
  => [STree]
  -> Typename
  -> ObjectDef m a
  -> V (Branch m a)
validate'Object t ty (ObjectDef fields) = fmap HashMap.fromList $ mapM (check . project) t
  where
    check s = checkTypename s *> checkField s
    checkTypename = maybe (pure ()) (typenameMatches ty) . typeConstraint . node
    checkField :: TreeF Selection STree -> V (Text, ([STree], Step m a))
    checkField s@(NodeF (Sel { name = "__typename", alias }) _)
      = validate'__typename s *> pure
        ( fromMaybe "__typename" alias
        , ([], Exists $ Kleisli $ const $ pure ty)
        )
    checkField (NodeF (Sel { name, alias, input }) tail) = case Map.lookup name fields of
      Nothing -> Left $ "Field " <> name <> " does not exist in object of type " <> ty
      Just (Some (FieldDef _ f)) -> do
        i <- readInput ("input of " <> ty) input
        pure (fromMaybe name alias, (tail, Exists . Kleisli . getCompose $ f i))

validate'Union :: forall m a
  .  Monad m
  => [STree]
  -> Typename
  -> UnionDef m a
  -> V (Variant m a)
validate'Union t ty (UnionDef types) = do
    t' <- groupSelection ty (Map.keys types) t
    Map.traverseWithKey (go t') types
  where
    go :: Map Typename [STree] -> Typename -> Case m a -> V (VariantCase m a)
    go t' k (Case f) = case Map.lookup k t' of
      Nothing  -> VariantCase f <$> pure (Branch HashMap.empty)
      Just t'' -> VariantCase f <$> validate t'' typeDef

validate'__typename :: TreeF Selection a -> V ()
validate'__typename (NodeF _ (_:_)) = Left "Field __typename cannot have a selection"
validate'__typename (NodeF (Sel { input }) _)
  | HashMap.null input = Right ()
  | otherwise          = Left "Field __typename does not have arguments"

typenameMatches :: Typename -> Typename -> V ()
typenameMatches t0 t1
  | t0 == t1  = Right ()
  | otherwise = Left $ "Typename mismatch: Expected " <> t0 <> ", got " <> t1

groupSelection :: Typename -> [Typename] -> [STree] -> V (Map Typename [STree])
groupSelection ty as = pure . Map.fromListWith (++) . join <=< mapM go
  where
    go :: STree -> V [(Typename, [STree])]
    go t =
      case ((typeConstraint &&& name) . node . project) t of
        (Nothing, "__typename") -> Right (fmap (\a -> (a, [t])) as)
        (Nothing, _) -> Left $ "Invalid selection with unspecified typename on union type " <> ty
        (Just a, _)
          | List.elem a as -> Right [(a, [t])]
          | otherwise      -> Left $ a <> " is not a possible type of union type " <> ty

node :: TreeF a b -> a
node (NodeF a _) = a
