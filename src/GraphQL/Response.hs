{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts #-}

module GraphQL.Response where

import           Control.Lens (Lens, Traversal)
import           Control.Monad.Error.Class (MonadError(..))
import qualified Data.Aeson as JSON
import           Data.Aeson (object, (.=))
import           Data.Bifoldable (Bifoldable)
import           Data.Bifunctor (Bifunctor)
import           Data.Bifunctor.Biff (Biff(..))
import           Data.Bitraversable (Bitraversable(..))
import           Data.Data (Data)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Text.Megaparsec.Error (ShowErrorComponent(..))

newtype Response e r
  = Response { getResponse :: Biff (,) (Compose Maybe NonEmpty) Maybe e r }
  deriving (Eq, Show, Functor, Foldable, Traversable, Bifunctor, Bifoldable, Bitraversable)

_errors :: Traversal (Response a r) (Response b r) a b
_errors f = bitraverse f pure

_data :: Traversal (Response e a) (Response e b) a b
_data = traverse

type ResponseJSON = Response JSON.Value JSON.Value

instance (JSON.ToJSON e, JSON.ToJSON r) => JSON.ToJSON (Response e r) where
  toJSON (Response (Biff (e, r))) = object ["data" .= r, "errors" .= e]

data Pos = Pos { line :: Int, column :: Int }
  deriving (Eq, Show, Ord, Data, Generic, JSON.ToJSON)

type Path = [Text]

data ErrorCode
  = SYNTAX_ERROR
  | VALIDATION_ERROR
  | BAD_INPUT_ERROR
  deriving (Eq, Show, Ord)

data GraphQLError
  = GraphQLError
    { errorCode :: ErrorCode
    , locations :: Maybe [Pos]
    , path :: Maybe [Text]
    , message :: Text
    }
  deriving (Eq, Show, Ord)

instance ShowErrorComponent GraphQLError where showErrorComponent = show

instance JSON.ToJSON GraphQLError where
  toJSON GraphQLError {..}
    = object
      [ "path" .= path
      , "locations" .= locations
      , "message" .= message
      , "extensions" .= object [ "code" .= show errorCode ]
      ]

graphQLError :: MonadError GraphQLError m => ErrorCode -> [Pos] -> Text -> m a
graphQLError errorCode locations message = throwError $ GraphQLError errorCode (Just locations) Nothing message

errorResponse :: NonEmpty e -> Response e a
errorResponse errors = Response (Biff (Compose (Just errors), Nothing))
