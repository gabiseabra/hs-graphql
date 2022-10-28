{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GraphQL.Resolver.Output
  ( resolveType
  ) where

import           Control.Applicative            ( (<|>) )
import           Control.Arrow                  ( (&&&)
                                                , Kleisli(..)
                                                )
import           Control.Category               ( Category(..) )
import           Control.Comonad.Cofree         ( Cofree(..) )
import           Control.Lens                   ( view )
import           Control.Lens.Indexed           ( itraverse )
import           Control.Monad                  ( (<=<)
                                                , join
                                                )
import           Control.Monad.Error.Class      ( MonadError(..) )
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Types              as JSON
                                                ( parse )
import           Data.Bitraversable             ( bisequence )
import           Data.Functor.Base              ( TreeF(..) )
import           Data.Functor.Identity          ( Identity(..) )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict            ( HashMap )
import           Data.Hashable                  ( Hashable(..) )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified Debug.Trace                   as Debug
import           GraphQL.Internal               ( Exists1(..)
                                                , Exists2(..)
                                                )
import qualified GraphQL.Parser.Types          as AST
import           GraphQL.Resolver.Input         ( Input
                                                , inputParser
                                                )
import qualified GraphQL.Response              as E
import           GraphQL.Response               ( Response(..) )
import           GraphQL.Schema
import           Prelude                 hiding ( (.)
                                                , id
                                                )

type V = Either (NonEmpty E.GraphQLError)

-- Resolve type with better inference
resolveType
  :: forall m a
   . Monad m
  => GraphQLOutputType m a
  => AST.ExecutableSelection
  -> V (Kleisli m a JSON.Value)
resolveType = resolveType' (typeDef @a)

resolveType'
  :: forall m a k
   . Monad m
  => k !! m
  => k !>> OUT
  => TypeDef k a
  -> AST.ExecutableSelection
  -> V (Kleisli m a JSON.Value)
resolveType' ScalarType {..} (pos :< NodeF _ (_ : _)) =
  E.graphQLError E.VALIDATION_ERROR [pos]
    $  "Scalar type "
    <> scalarTypename
    <> " cannot have a selection"
resolveType' EnumType {..} (pos :< NodeF _ (_ : _)) =
  E.graphQLError E.VALIDATION_ERROR [pos]
    $  "Enum type "
    <> enumTypename
    <> " cannot have a selection"
resolveType' ObjectType {..} (pos :< NodeF _ []) =
  E.graphQLError E.VALIDATION_ERROR [pos]
    $  "Object type "
    <> objectTypename
    <> " must have a selection"
resolveType' UnionType {..} (pos :< NodeF _ []) =
  E.graphQLError E.VALIDATION_ERROR [pos]
    $  "Union type "
    <> unionTypename
    <> " must have a selection"
resolveType' ScalarType{}  _ = pure . Kleisli $ pure . JSON.toJSON
resolveType' EnumType {..} _ = pure . Kleisli $ pure . JSON.String . encodeEnum
resolveType' def@PureType {..} s =
  Kleisli . fmap (pure . runIdentity) . runKleisli <$> resolveType' pureInnerType s
resolveType' def@ListType {..} s =
  fmap JSON.toJSON1 . traverseK <$> resolveType' @m listInnerType s
resolveType' def@NullableType {..} s =
  fmap JSON.toJSON1 . traverseK <$> resolveType' @m nullableInnerType s
resolveType' def@ObjectType {..} s = def `object` s
resolveType' def@UnionType {..}  s = def `union` s

object
  :: forall m a
   . Monad m
  => TypeDef (OBJECT @m) a
  -> AST.ExecutableSelection
  -> V (Kleisli m a JSON.Value)
object def@ObjectType {..} s@(pos :< NodeF _ as) = do
  validateTypename objectTypename s
  fmap JSON.Object
    .   sequence
    .   HashMap.fromList
    <$> traverse (sequence . (k &&& v)) as
 where
  k (_ :< NodeF field _) = fieldName' field
  v s = runResolver s =<< select def s <* validateTypename objectTypename s

runResolver
  :: forall m a r
   . Monad m
  => AST.ExecutableSelection
  -> Resolver m a
  -> V (Kleisli m a JSON.Value)
runResolver s@(pos :< NodeF AST.Field {..} _) (Exists2 (Field {..} :: Field
    (Kleisli m a)
    i
    o))
  = do
    fab <- fieldResolver <$> readInput @i pos fieldArgs
    fbr <- resolveType' (typeDef @o) s
    pure $ fbr . fab

select
  :: forall m k a
   . Applicative m
  => TypeDef (OBJECT @m) a
  -> AST.ExecutableSelection
  -> V (Resolver m a)
select ObjectType {..} (pos :< NodeF field@AST.Field { fieldName = "__typename", ..} _)
  | not $ HashMap.null fieldArgs
  = E.graphQLError E.VALIDATION_ERROR
                   [pos]
                   "Field __typename does not have arguments"
  | otherwise
  = pure . Exists2 . Field mempty $ \() ->
    Kleisli . const . pure $ objectTypename
select ObjectType {..} (pos :< NodeF field@AST.Field {..} _) =
  maybe
      (  E.graphQLError E.VALIDATION_ERROR [pos]
      $  objectTypename
      <> " does not have a field named \""
      <> fieldName
      <> "\""
      )
      pure
    . flip Map.lookup objectFields
    $ fieldName

union
  :: forall m a
   . Monad m
  => TypeDef (UNION @m) a
  -> AST.ExecutableSelection
  -> V (Kleisli m a JSON.Value)
union def@UnionType {..} (pos :< NodeF field as) = do
  as' <- HashMap.fromListWith (++) . join <$> traverse match as
  fmap JSON.toJSON . sum . HashMap.elems <$> itraverse next as'
 where
  match a = fmap (, [a]) <$> matchType def a
  next (Hash _ v) s = runVariant (pos :< NodeF field s) v
  sum :: forall a b . [Kleisli m a (Maybe b)] -> Kleisli m a (Maybe b)
  sum []       = pure Nothing
  sum (f : fs) = f >>= \case
    Nothing -> sum fs
    b       -> pure b

runVariant
  :: forall m a r
   . Monad m
  => AST.ExecutableSelection
  -> Variant m a
  -> V (Kleisli m a (Maybe JSON.Value))
runVariant s (Exists1 (Kleisli f :: Kleisli Maybe a b)) = do
  Kleisli g <- resolveType' (typeDef @b) s
  pure $ Kleisli \a -> sequence (g <$> f a)

matchType
  :: TypeDef (UNION @m) a -> AST.ExecutableSelection -> V [Hash (Variant m a)]
matchType UnionType {..} (pos :< NodeF AST.Field {..} _)
  | fieldName == "__typename"
  = pure . fmap (uncurry Hash) . Map.toList $ unionPossibleTypes
  | Just t <- fieldTypename
  = case Map.lookup t unionPossibleTypes of
    Just a -> pure [Hash t a]
    Nothing ->
      E.graphQLError E.VALIDATION_ERROR [pos]
        $  "\""
        <> t
        <> "\" is not a possible type of "
        <> unionTypename
  | otherwise
  = E.graphQLError E.VALIDATION_ERROR
                   [pos]
                   "Selections of union types must have a typename"

data Hash a = Hash Text a

instance Eq (Hash a) where
  (Hash a _) == (Hash b _) = a == b
instance Hashable (Hash a) where
  hashWithSalt s (Hash a _) = hashWithSalt s a

fieldName' :: AST.Field a -> Text
fieldName' f = fromMaybe (AST.fieldName f) (AST.fieldAlias f)

validateTypename :: Typename -> AST.ExecutableSelection -> V ()
validateTypename t0 (pos :< NodeF AST.Field {..} _)
  | Just t1 <- fieldTypename
  , t1 /= t0
  = E.graphQLError E.VALIDATION_ERROR [pos]
    $  "Typename mismatch: Expected "
    <> t0
    <> ", got "
    <> t1
  | otherwise
  = pure ()

readInput :: forall a . GraphQLInput a => E.Pos -> Input -> V a
readInput pos i = case JSON.parse inputParser i of
  JSON.Success a -> pure a
  JSON.Error   e -> E.graphQLError E.BAD_INPUT_ERROR [pos] $ Text.pack e

traverseK
  :: (Applicative m, Traversable f) => Kleisli m a b -> Kleisli m (f a) (f b)
traverseK (Kleisli f) = Kleisli (traverse f)
