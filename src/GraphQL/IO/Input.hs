{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}

module GraphQL.IO.Input
  ( Input
  , inputTypeParser
  , inputFieldsParser
  , inputParser
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens (view)
import           Control.Monad ((<=<))
import           Control.Monad.Error.Class (MonadError(..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import           Data.Row.Records (Rec, Row)
import qualified Data.Text as Text
import           Data.Text (Text)
import           GraphQL.TypeSystem.Main

type Input = HashMap Text JSON.Value

showType :: JSON.Value -> Text
showType JSON.Null      = "Null"
showType JSON.String {} = "String"
showType JSON.Number {} = "Number"
showType JSON.Bool {}   = "Boolean"
showType JSON.Array {}  = "Array"
showType JSON.Object {} = "Object"

inputTypeParser :: TypeDef k a -> JSON.Value -> JSON.Parser a
inputTypeParser def@ScalarType {..} = fmap (prependFailure def) JSON.parseJSON
inputTypeParser def@EnumType {..} = fmap (prependFailure def) $ \case
  JSON.String val -> case Map.lookup val enumValues of
    Just EnumValue {..} -> pure enumValue
    Nothing -> fail . Text.unpack $ "\"" <> val <> "\" is not a valid value of " <> enumTypename
  val -> JSON.typeMismatch "String" val
inputTypeParser def@ListType {..} = traverse (inputTypeParser listInnerType) <=< fmap (prependFailure def) JSON.parseJSON1
inputTypeParser def@NullableType {..} = traverse (inputTypeParser nullableInnerType) <=< fmap (prependFailure def) JSON.parseJSON1
inputTypeParser def@InputObjectType {..} = pure . decodeInputObject <=< fmap (prependFailure def) inputFieldsParser
inputTypeParser def = const . prependFailure def . fail $ show (kindOf def) <> " is not a valid kind of input"

prependFailure t
  = JSON.prependFailure
  $ "Failed to parse "
  <> kindOf t <> " "
  <> Text.unpack (view _typename t) <> ": "

inputFieldsParser :: forall r
  .  Row.AllUniqueLabels r
  => Row.Forall r GraphQLInputType
  => JSON.Value
  -> JSON.Parser (Rec r)
inputFieldsParser (JSON.Object obj) = Rec.fromLabelsA @GraphQLInputType readField
  where
    readField :: forall l a. (Row.KnownSymbol l, GraphQLInputType a) => Row.Label l -> JSON.Parser a
    readField lbl =
      let
        key = Text.pack (show lbl)
        val = fromMaybe JSON.Null $ HashMap.lookup key obj
      in inputTypeParser (typeDef @a) val
inputFieldsParser val = JSON.typeMismatch "Object" val

inputParser :: forall a. GraphQLInput a => Input -> JSON.Parser a
inputParser = case inputDef @a of
  EmptyFields a -> const $ pure a
  InputFields f -> fmap f . inputFieldsParser . JSON.Object
