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
                                                , (>>>)
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
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
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
import           Data.Proxy                     ( Proxy )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq
                                                , (|>)
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified Debug.Trace                   as Debug
import           GraphQL.Internal
import qualified GraphQL.Parser.Types          as AST
import           GraphQL.Resolver.Input         ( Input
                                                , inputParser
                                                )
import qualified GraphQL.Response              as E
import           GraphQL.Schema
import           Prelude                 hiding ( (.)
                                                , id
                                                )
import Data.Foldable (toList)
import Data.Functor ((<&>))

type V = ReaderT ExecutionContext (Either (NonEmpty E.GraphQLError))

graphQLError :: E.ErrorCode -> Text -> V a
graphQLError errorCode msg = do
  pos <- Just . pure <$> reader currentPos
  path <- reader (toList . path) <&> \case
            [] -> Nothing
            as -> Just as
  throwError . pure $ E.GraphQLError errorCode pos path msg

data ExecutionContext = ExecutionContext
  { currentPos    :: E.Pos
  , parentContext :: Maybe (AST.Field JSON.Value, ExecutionContext)
  }

localCtx :: AST.ExecutableSelection -> V a -> V a
localCtx (pos :< NodeF field _) = local ctx
  where ctx = ExecutionContext pos . Just . (field, )

path :: ExecutionContext -> Seq Text
path ExecutionContext {..} = case parentContext of
  Just (AST.Field {..}, parent) ->
    path parent |> fromMaybe fieldName fieldAlias
  Nothing -> mempty

-- Runs en executable selection against an output type `a`'s type definition,
-- returning a function that executes the selection against an actual instance
-- of `a`, or an error if selected fields can't be matched.
resolveType
  :: forall m a
   . Monad m
  => GraphQLOutputType m a
  => AST.ExecutableSelection
  -> Either (NonEmpty E.GraphQLError) (Kleisli m a JSON.Value)
resolveType s@(pos :< _) = resolveType' (typeDef @a) s `runReaderT` ctx
  where ctx = ExecutionContext pos Nothing

resolveType'
  :: forall m a k
   . Monad m
  => k !! m
  => k !>> OUT
  => TypeDef k a
  -> AST.ExecutableSelection
  -> V (Kleisli m a JSON.Value)
resolveType' ScalarType {..} (pos :< NodeF _ (_ : _)) =
  graphQLError E.VALIDATION_ERROR
    $  "Scalar type "
    <> scalarTypename
    <> " cannot have a selection"
resolveType' EnumType {..} (pos :< NodeF _ (_ : _)) =
  graphQLError E.VALIDATION_ERROR
    $  "Enum type "
    <> enumTypename
    <> " cannot have a selection"
resolveType' ObjectType {..} (pos :< NodeF _ []) =
  graphQLError E.VALIDATION_ERROR
    $  "Object type "
    <> objectTypename
    <> " must have a selection"
resolveType' UnionType {..} (pos :< NodeF _ []) =
  graphQLError E.VALIDATION_ERROR
    $  "Union type "
    <> unionTypename
    <> " must have a selection"
resolveType' ScalarType{}  _ = pure . Kleisli $ pure . JSON.toJSON
resolveType' EnumType {..} _ = pure . Kleisli $ pure . JSON.String . encodeEnum
resolveType' def@ListType {..} s =
  fmap JSON.toJSON1 . traverseK <$> resolveType' @m listInnerType s
resolveType' def@NullableType {..} s =
  fmap JSON.toJSON1 . traverseK <$> resolveType' @m nullableInnerType s
resolveType' def@PureType {..} s =
  Kleisli
    .   fmap (pure . runIdentity)
    .   runKleisli
    <$> resolveType' pureInnerType s
resolveType' def@ObjectType {..} s = def `object` s
resolveType' def@UnionType {..}  s = def `union` s

-- * Object kind

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
  v s =
    localCtx s
      $   runResolver s
      =<< select def s
      <*  validateTypename objectTypename s

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

__typename :: Applicative m => Text -> Resolver m a
__typename a = Exists2 . Field Nothing . fmap Kleisli $ \() _ -> pure a

select
  :: forall m k a
   . Applicative m
  => TypeDef (OBJECT @m) a
  -> AST.ExecutableSelection
  -> V (Resolver m a)
select ObjectType {..} (pos :< NodeF field@AST.Field { fieldName = "__typename", ..} _)
  = pure $ __typename objectTypename
select ObjectType {..} (pos :< NodeF field@AST.Field {..} _) =
  maybe
      (  graphQLError E.VALIDATION_ERROR
      $  objectTypename
      <> " does not have a field named \""
      <> fieldName
      <> "\""
      )
      pure
    . flip Map.lookup objectFields
    $ fieldName

-- * Union kind

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
  match s = fmap (, [s]) <$> localCtx s (matchType def s)
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
      graphQLError E.VALIDATION_ERROR
        $  "\""
        <> t
        <> "\" is not a possible type of "
        <> unionTypename
  | otherwise
  = graphQLError E.VALIDATION_ERROR "Selections of union types must have a typename"

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
  = graphQLError E.VALIDATION_ERROR
    $  "Typename mismatch: Expected "
    <> t0
    <> ", got "
    <> t1
  | otherwise
  = pure ()

readInput :: forall a . GraphQLInput a => E.Pos -> Input -> V a
readInput pos i = case JSON.parse inputParser i of
  JSON.Success a -> pure a
  JSON.Error   e -> graphQLError E.BAD_INPUT_ERROR $ Text.pack e

traverseK
  :: (Applicative m, Traversable f) => Kleisli m a b -> Kleisli m (f a) (f b)
traverseK (Kleisli f) = Kleisli (traverse f)
