{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleContexts #-}

module GraphQL.Response where

import           Control.Lens                   ( Lens
                                                , Traversal
                                                )
import           Control.Monad.Error.Class      ( MonadError(..) )
import qualified Data.Aeson                    as JSON
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                , object
                                                )
import           Data.Bifoldable                ( Bifoldable )
import           Data.Bifunctor                 ( Bifunctor )
import           Data.Bifunctor.Biff            ( Biff(..) )
import           Data.Bitraversable             ( Bitraversable(..) )
import           Data.Data                      ( Data )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Functor.Identity          ( Identity(..) )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict            ( HashMap )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec.Error          ( ShowErrorComponent(..) )

data Request = Request
  { reqQuery         :: Text
  , reqOperationName :: Maybe Text
  , reqVariables     :: Maybe JSON.Object
  }
  deriving (Eq, Show)

instance JSON.FromJSON Request where
  parseJSON = JSON.withObject "Request" $ \val ->
    Request <$> val .: "query" <*> val .: "operationName" <*> val .: "variables"

data Response = Response
  { resErrors :: Maybe (NonEmpty GraphQLError)
  , resData   :: Maybe JSON.Object
  }
  deriving (Eq, Show)

instance JSON.ToJSON Response where
  toJSON Response {..} = object ["errors" .= resErrors, "data" .= resData]

data Pos = Pos
  { line   :: Int
  , column :: Int
  }
  deriving (Eq, Show, Ord, Data, Generic, JSON.ToJSON)

type Path = [Text]

data ErrorCode
  = SYNTAX_ERROR
  | VALIDATION_ERROR
  | BAD_INPUT_ERROR
  | EXECUTION_ERROR
  deriving (Eq, Show, Ord)

data GraphQLError = GraphQLError
  { errorCode      :: ErrorCode
  , errorLocations :: Maybe [Pos]
  , errorPath      :: Maybe [Text]
  , errorMessage   :: Text
  }
  deriving (Eq, Show, Ord)

instance ShowErrorComponent GraphQLError where
  showErrorComponent = show

instance JSON.ToJSON GraphQLError where
  toJSON GraphQLError {..} = object
    [ "path" .= errorPath
    , "locations" .= errorLocations
    , "message" .= errorMessage
    , "extensions" .= object ["code" .= show errorCode]
    ]

graphQLError
  :: MonadError (NonEmpty GraphQLError) m => ErrorCode -> [Pos] -> Text -> m a
graphQLError errorCode locations message =
  throwError . pure $ GraphQLError errorCode (Just locations) Nothing message
