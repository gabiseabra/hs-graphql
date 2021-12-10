{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GraphQL.IO.Output
  ( resolve
  , resolveType
  , ExecutableSelection
  ) where

import           Control.Applicative ((<|>), liftA2)
import           Control.Arrow (Kleisli(..), (&&&), (<<<), (>>>))
import           Control.Comonad.Cofree (Cofree(..), unwrap)
import qualified Control.Comonad.Trans.Cofree as CofreeT
import           Control.Monad ((>=>), (<=<), join)
import qualified Data.Aeson as JSON
import           Data.Bifunctor (first, second)
import           Data.Bitraversable (bisequence)
import           Data.Fix (Fix)
import           Data.Functor (($>))
import           Data.Functor.Base (TreeF(..))
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Foldable (Recursive(..), Corecursive(..), Base)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import qualified Data.Row as Row
import           Data.Row (Rec)
import qualified Data.Row.Records as Rec
import qualified Data.Text as Text
import           Data.Text (Text)
import           GraphQL.AST.Document (Name, Field(..), ExecutableSelection)
import           GraphQL.IO.Input ( readInput )
import qualified GraphQL.Response as E
import           GraphQL.Response (V, Response(..))
import           GraphQL.Types ()
import           GraphQL.TypeSystem

type Selection = [ExecutableSelection]

data NodeType = LEAF | BRANCH | VARIANT | WRAPPER NodeType

data Step m a where
  Step ::
    ( GraphQLOutputType m o
    ) =>
    { selection :: Selection
    , step :: a -> m o
    } -> Step m a

type Branch m a = HashMap Text (Step m a)

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

runStep :: forall m a . Monad m => Step m a -> V (a -> m JSON.Value)
runStep Step {step, selection} = (step >=>) <$> resolveType selection

resolve :: forall k m a. (Monad m, k !! m) => TypeDef k a -> [ExecutableSelection] -> V (a -> m JSON.Value)
resolve t = sequenceResolver <=< flip mkResolution t

resolveType :: forall m a. (Monad m, GraphQLOutputType m a) => [ExecutableSelection] -> V (a -> m JSON.Value)
resolveType = resolve $ typeDef @a

-- resolveRoot :: forall op m r a . (Monad m, GraphQLRootType op m r a) => NonEmpty ExecutableSelection -> V (a -> m r)
-- resolveRoot t = case typeDef @a of
--   (RootType ty (QueryDef f fields)) -> (f <=<) <$> mkResponse . resolve (ObjectType ty fields :: TypeDef (OBJECT @m) a)
--   -- (RootType ty (MutationDef f fields)) -> (f <=<) <$> resolveType (ObjectType ty fields) (NE.toList t)

mkResponse :: Monad m => V (a -> m JSON.Value) -> a -> m Response
mkResponse (Left errors) = const . pure $ Response JSON.Null (Just errors)
mkResponse (Right f) = f >=> pure . flip Response Nothing

sequenceResolver :: forall m k a
  .  Monad m
  => Resolution k m a
  -> V (a -> m JSON.Value)
sequenceResolver (Leaf f) = pure f
-- TODO — if it's an branch wrapper, batch together all the same fields
sequenceResolver (Wrapper h r) = g <$> sequenceResolver r
  where g f = pure . h <=< traverse f
sequenceResolver (Branch as) = g <$> mapM runStep as
  where g as = pure . JSON.Object <=< sequence2 as
sequenceResolver (Variant as)
  = (<<<) (fromMaybe $ error "Unexpected return type from union type")
  <$> foldr ((liftA2 . liftA2) (<|>) . g) (pure $ const Nothing) as
  where g (VariantCase h a) = (. h) . fmap <$> sequenceResolver a

sequence2 :: (Traversable t, Monad m, Monad f) => t (f (m a)) -> f (m (t a))
sequence2 = fmap sequence . sequence

mkResolution :: forall k m a
  .  Monad m
  => k !! m
  => [ExecutableSelection]
  -> TypeDef k a
  -> V (Resolution (NodeTypeOf k) m a)
mkResolution []      (ScalarType   _  _ def                ) = pure (Leaf $ pure . encodeScalar def)
mkResolution []      (EnumType     _  _ def                ) = pure (Leaf $ pure . JSON.toJSON . encodeEnum def)
mkResolution t       (ListType     _  k (ListDef     enc _)) = Wrapper (JSON.toJSON1 . enc) <$> mkResolution t k
mkResolution t       (NullableType _  k (NullableDef enc _)) = Wrapper (JSON.toJSON1 . enc) <$> mkResolution t k
mkResolution t@(_:_) (ObjectType   ty _ def                ) = Branch <$> validateObject t ty def
mkResolution t@(_:_) (UnionType ty _ def) = Variant <$> validateUnion t ty def
-- mkResolution t@(_:_) (RootType ty ()) = Branch <$> validateUnion t ty def
mkResolution _       (ScalarType   ty _ _                  ) = E.validationError [] $ "Scalar type " <> ty <> " cannot have a selection"
mkResolution _       (EnumType     ty _ _                  ) = E.validationError [] $ "Enum type " <> ty <> " cannot have a selection"
mkResolution _       (ObjectType   ty _ _                  ) = E.validationError [] $ "Object type " <> ty <> " must have a selection"
mkResolution _       (UnionType    ty _ _                  ) = E.validationError [] $ "Union type " <> ty <> " must have a selection"
mkResolution _       (RootType     ty _                    ) = E.validationError [] $ "Root type " <> ty <> " must have a selection"
mkResolution _       (InputObjectType ty _ _               ) = E.validationError [] $ "Input object type " <> ty <> " is not selectable"

validateObject :: forall m a
  .  Monad m
  => [ExecutableSelection]
  -> Typename
  -> ObjectDef m a
  -> V (Branch m a)
validateObject t ty (ObjectDef fields) = HashMap.fromList <$> mapM go t
  where
    go s = do typenameMatches ty s; checkField s
    checkField :: ExecutableSelection -> V (Text, Step m a)
    checkField s@(_ :< NodeF field@Field { fieldName = "__typename" } _)
      = validateTypenameField s $> (getFieldName field, Step [] (const . pure $ ty))
    checkField (pos :< NodeF field@Field { fieldName, fieldArgs } tail) = case Map.lookup fieldName fields of
      Nothing -> E.validationError [pos] $ "Field " <> fieldName <> " does not exist in object of type " <> ty
      Just (Some2 (FieldDef _ f)) -> do
        i <- first (pure . E.ValidationError [pos]) $ readInput ("input of " <> ty) fieldArgs
        pure (getFieldName field, Step tail (getCompose $ f i))

getFieldName :: Field a -> Name
getFieldName Field {fieldName, fieldAlias} = fromMaybe fieldName fieldAlias

validateTypenameField :: ExecutableSelection -> V ()
validateTypenameField (pos :< NodeF _ (_ : _)) = E.validationError [pos] "Field __typename cannot have a selection"
validateTypenameField (pos :< NodeF Field {fieldArgs} _)
  | HashMap.null fieldArgs = Right ()
  | otherwise = E.validationError [pos] "Field __typename does not have arguments"

typenameMatches :: Typename -> ExecutableSelection -> V ()
typenameMatches t0 (pos :< NodeF Field {fieldType} _)
  | Just (t1, False) <- (id &&& (==) t0) <$> fieldType = E.validationError [pos] $ "Typename mismatch: Expected " <> t0 <> ", got " <> t1
  | otherwise = pure ()

validateUnion :: forall m a
  .  Monad m
  => [ExecutableSelection]
  -> Typename
  -> UnionDef m a
  -> V (Variant m a)
validateUnion t ty (UnionDef types) = do
    t' <- groupSelection ty (Map.keys types) t
    Map.traverseWithKey (go t') types
  where
    go :: Map Typename [ExecutableSelection] -> Typename -> Case m a -> V (VariantCase m a)
    go t' k (Case f) = case Map.lookup k t' of
      Nothing  -> VariantCase f <$> pure (Branch HashMap.empty)
      Just t'' -> VariantCase f <$> mkResolution t'' typeDef

groupSelection :: Typename -> [Typename] -> [ExecutableSelection] -> V (Map Typename [ExecutableSelection])
groupSelection ty as = pure . Map.fromListWith (++) . join <=< mapM go
  where
    go :: ExecutableSelection -> V [(Typename, [ExecutableSelection])]
    go t@(pos :< NodeF Field {fieldType, fieldName} _) = case (fieldType, fieldName) of
      (Nothing, "__typename") -> pure $ fmap (, [t]) as
      (Nothing, _) -> E.validationError  [pos] $ "Invalid selection with unspecified typename on union type " <> ty
      (Just a, _)
        | a `List.elem` as -> pure [(a, [t])]
        | otherwise        -> E.validationError [pos] $ a <> " is not a possible type of union type " <> ty
