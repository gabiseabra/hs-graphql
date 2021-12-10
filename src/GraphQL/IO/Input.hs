{-# LANGUAGE
    DataKinds
  , GADTs
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  , TypeApplications
  , ScopedTypeVariables
  , DefaultSignatures
  , TypeOperators
  , OverloadedStrings
  , RankNTypes
#-}

module GraphQL.IO.Input
  ( Input
  , readInputType
  , readInputFields
  , readInput
  ) where

import           Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Aeson.Types ((<?>))
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import           Data.Row.Records (Rec, Row)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Vector (Vector)
import           GraphQL.AST.Document (Name)
import           GraphQL.TypeSystem.Main
    ( EnumDef(EnumDef),
      GraphQLInput(..),
      GraphQLInputType,
      GraphQLType(..),
      InputDef(..),
      InputObjectDef(..),
      ListDef(..),
      NullableDef(..),
      ScalarDef(..),
      TypeDef(..) )

type Input = HashMap Name JSON.Value

showType :: JSON.Value -> Text
showType JSON.Null       = "Null"
showType (JSON.String _) = "String"
showType (JSON.Number _) = "Number"
showType (JSON.Bool   _) = "Boolean"
showType (JSON.Array  _) = "Array"
showType (JSON.Object _) = "Object"

readScalarType :: forall a. ScalarDef a -> Text -> JSON.Value -> Either Text a
readScalarType (ScalarDef _ dec) lbl val = maybe err pure $ dec val
  where err = Left $ "Failed to read " <> lbl <> ". Unexpected " <> showType val

readEnumType :: forall a. EnumDef a -> Text -> JSON.Value -> Either Text a
readEnumType (EnumDef _ _ dec) lbl (JSON.String val) = maybe err pure $ dec val
  where err = Left $ "Failed to read " <> lbl <> ". " <> Text.pack (show val) <> " is not a valid enum value"
readEnumType _   lbl val = Left $ "Failed to read " <> lbl <> ". Expected a String value but got " <> showType val

readListType :: forall f. ListDef f -> Text -> JSON.Value -> Either Text (f JSON.Value)
readListType (ListDef _ dec) lbl (JSON.Array val) = maybe err pure $ dec val
  where err = Left $ "Failed to read " <> lbl
readListType _   lbl val = Left $ "Failed to read " <> lbl <> ". Expected an Array value but got " <> showType val

readNullableType :: forall f. NullableDef f -> JSON.Value -> Either Text (f JSON.Value)
readNullableType (NullableDef _ dec) JSON.Null = pure (dec Nothing)
readNullableType (NullableDef _ dec) val       = pure (dec $ Just val)

readInputType :: TypeDef k a -> JSON.Value -> Either Text a
readInputType (ScalarType      ty _ def                   ) = readScalarType def ty
readInputType (EnumType        ty _ def                   ) = readEnumType def ty
readInputType (ListType        ty a def@(ListDef      _ _)) = traverse (readInputType a) <=< readListType def ty
readInputType (NullableType    _  a def@(NullableDef  _ _)) = traverse (readInputType a) <=< readNullableType def
readInputType (InputObjectType ty _ (InputObjectDef f    )) = fmap f . readInputFields ty
readInputType (ObjectType      ty _ _                     ) = const . Left $ "Object type " <> ty <> "is not an input type"
readInputType (UnionType       ty _ _                     ) = const . Left $ "Union type " <> ty <> "is not an input type"
readInputType (RootType        ty _                       ) = const . Left $ "Root type " <> ty <> "is not an input type"

readInputFields :: forall r
  .  Row.AllUniqueLabels r
  => Row.Forall r GraphQLInputType
  => Text
  -> JSON.Value
  -> Either Text (Rec r)
readInputFields _ (JSON.Object obj) = Rec.fromLabelsA @GraphQLInputType readField
  where
    readField :: forall l a. (Row.KnownSymbol l, GraphQLInputType a) => Row.Label l -> Either Text a
    readField lbl =
      let
        key = Text.pack (show lbl)
        val = fromMaybe JSON.Null $ Map.lookup key obj
      in readInputType (typeDef @a) val
readInputFields lbl val = Left $ "Failed to read " <> lbl <> ". Expected an Object value but got " <> showType val

readInput :: forall a. GraphQLInput a => Text -> Input -> Either Text a
readInput lbl = case inputDef @a of
  EmptyFields a                  -> const $ pure a
  InputFields (InputObjectDef f) -> pure . f <=< readInputFields lbl . JSON.Object
