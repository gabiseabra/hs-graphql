{-# LANGUAGE
    OverloadedStrings
#-}

module GraphQL.Response where

import qualified Data.Aeson as JSON
import Data.Aeson (object, (.=))
import GraphQL.AST.Document (Name, Pos)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))
import Text.Megaparsec.Error (ShowErrorComponent(..))

data Response
  = Response
    { resData :: JSON.Value
    , resErrors :: Maybe (NonEmpty GraphQLError)
    } deriving (Eq, Show)

instance JSON.ToJSON Response where
  toJSON (Response a e) = object ["data" .= a, "errors" .= e]

type Path = [Text]

data GraphQLError
  = ParseError [Pos] Text
  | ValidationError [Pos] Text
  | RuntimeError [Pos] Text Path
  deriving (Eq, Show, Ord)

instance ShowErrorComponent GraphQLError where showErrorComponent = show

instance JSON.ToJSON GraphQLError where
  toJSON (ParseError      pos msg     ) = object ["locations" .= pos, "message" .= msg]
  toJSON (ValidationError pos msg     ) = object ["locations" .= pos, "message" .= msg]
  toJSON (RuntimeError    pos msg path) = object ["locations" .= pos, "message" .= msg, "path" .= path]

type V = Either (NonEmpty GraphQLError)

validationError :: [Pos] -> Text -> V a
validationError pos msg = Left $ ValidationError pos msg :| []

parseError :: [Pos] -> Text -> V a
parseError pos msg = Left $ ParseError pos msg :| []

errorResponse :: NonEmpty GraphQLError -> Response
errorResponse errors = Response JSON.Null (Just errors)
