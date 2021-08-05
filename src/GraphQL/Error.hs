module GraphQL.Error where

import GraphQL.AST.Document (Name, Location)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))

data GraphQLError
  = ParseError [Location] Text
  | ValidationError [Location] Text
  | ExecutionError Location Name Text
  deriving (Eq, Show, Ord)

type V = Either (NonEmpty GraphQLError)

validationError :: [Location] -> Text -> V a
validationError loc msg = Left $ ValidationError loc msg :| []

parseError :: [Location] -> Text -> V a
parseError loc msg = Left $ ParseError loc msg :| []
