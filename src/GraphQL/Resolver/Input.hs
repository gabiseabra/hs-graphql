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

module GraphQL.Resolver.Input
  ( Input
  , inputTypeParser
  , inputFieldsParser
  , inputParser
  ) where

import           Control.Applicative            ( (<|>) )
import           Control.Arrow                  ( (>>>) )
import           Control.Lens                   ( view )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.Error.Class      ( MonadError(..) )
import           Data.Aeson                     ( (.:) )
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Types              as JSON
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict            ( HashMap )
import           Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Row                      as Row
import qualified Data.Row.Records              as Rec
import           Data.Row.Records               ( Rec
                                                , Row
                                                )
import           Data.String                    ( fromString )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           GraphQL.Schema

type Input = JSON.Object

-- Parses a field's input from JSON. Notes:
-- - Input fields that are not declared in the haskell type are ignored.
-- - The variable type names declared in the query are already forgotten at this
--   point, so they aren't validated against haskell types. You could legally
--   pass i.e. an enum variable to a string field and it wouldn't throw an error
--   as long as the JSON parser succeeds.
-- @spec http://spec.graphql.org/June2018/#sec-Coercing-Field-Arguments
inputParser :: forall a . GraphQLInput a => Input -> JSON.Parser a
inputParser = case inputDef @a of
  EmptyFields a -> const $ pure a
  InputFields f -> fmap f . inputFieldsParser . JSON.Object

-- Creates a JSON parser from a type definition
inputTypeParser :: TypeDef k a -> JSON.Value -> JSON.Parser a
inputTypeParser def@ScalarType {..} = fmap (prependFailure def) JSON.parseJSON
inputTypeParser def@EnumType {..}   = fmap (prependFailure def) $ \case
  JSON.String val -> case Map.lookup val enumValues of
    Just EnumValue {..} -> pure enumValue
    Nothing ->
      fail
        .  Text.unpack
        $  "\""
        <> val
        <> "\" is not a valid value of "
        <> enumTypename
  val -> JSON.typeMismatch "String" val
inputTypeParser def@ListType {..} =
  traverse (inputTypeParser listInnerType)
    <=< fmap (prependFailure def) JSON.parseJSON1
inputTypeParser def@NullableType {..} =
  traverse (inputTypeParser nullableInnerType)
    <=< fmap (prependFailure def) JSON.parseJSON1
inputTypeParser def@InputObjectType {..} =
  pure . decodeInputObject <=< fmap (prependFailure def) inputFieldsParser
inputTypeParser def =
  const
    .  prependFailure def
    .  fail
    $  show (kindOf def)
    <> " is not a valid kind of input"

prependFailure t =
  JSON.prependFailure
    $  "Failed to parse "
    <> kindOf t
    <> " "
    <> Text.unpack (view _typename t)
    <> ": "

-- Creates a JSON parser for input object fields
inputFieldsParser
  :: forall r
   . Row.AllUniqueLabels r
  => Row.Forall r GraphQLInputType => JSON.Value -> JSON.Parser (Rec r)
inputFieldsParser (JSON.Object obj) = Rec.fromLabelsA @GraphQLInputType
  readField
 where
  readField
    :: forall l a
     . (Row.KnownSymbol l, GraphQLInputType a)
    => Row.Label l
    -> JSON.Parser a
  readField lbl =
    let key = fromString (show lbl)
        val = obj .: key <|> pure JSON.Null
    in  inputTypeParser (typeDef @a) =<< val
inputFieldsParser val = JSON.typeMismatch "Object" val
