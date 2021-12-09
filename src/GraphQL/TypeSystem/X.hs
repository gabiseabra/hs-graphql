{-# LANGUAGE
    PolyKinds
  , TypeApplications
  , ScopedTypeVariables
#-}

module GraphQL.TypeSystem.X where

import GraphQL.TypeSystem.Main

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data Schema m r where
  Schema ::
    ( GraphQLRootType QUERY m r query
    , GraphQLRootType MUTATION m r mutation
    , GraphQLRootType SUBSCRIPTION m r subscription
    ) =>
    { query :: query
    , mutation :: mutation
    , subscription :: subscription
    } -> Schema m r

runOperation :: Schema m r -> ExecutableOperation -> m r

data Location
  = FieldName Text
  | Typename Text

type Path = [Location]

resolveOutput :: forall t m . Some2 (FieldDef t m) -> Some2 TypeDef
resolveOutput (Some2 (FieldDef {} :: FieldDef t m i a)) = Some2 (typeDef @a)

unionType :: forall m a . Case m a -> Some2 TypeDef
unionType (Case (f :: a -> Maybe b)) = Some2 (typeDef @b)

lookupField :: Path -> TypeDef k a -> Maybe (Some2 TypeDef)
lookupField [] ty = Just (Some2 ty)
lookupField xs (ListType _ ty _) = lookupField xs ty
lookupField xs (NullableType _ ty _) = lookupField xs ty
lookupField (FieldName name:xs) (ObjectType _ _ def) =
  runSome2 (lookupField xs) . resolveOutput =<< Map.lookup name (objectFields def)
lookupField (Typename name:xs) (UnionType _ _ def) =
  runSome2 (lookupField xs) . unionType =<< Map.lookup name (unionTypes def)
lookupField (Typename name:xs) ty =
  case typename ty == name of True -> lookupField xs ty; False -> Nothing
lookupField _ _ = Nothing
