module GraphQL.Error where

import GraphQL.AST.Document (Name, Pos)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import Text.Megaparsec.Error (ShowErrorComponent(..))

data GraphQLError
  = ParseError [Pos] Text
  | ValidationError [Pos] Text
  | ExecutionError Pos [Name] Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent GraphQLError where showErrorComponent = show

type V = Either (NonEmpty GraphQLError)

validationError :: [Pos] -> Text -> V a
validationError pos msg = Left $ ValidationError pos msg :| []

parseError :: [Pos] -> Text -> V a
parseError pos msg = Left $ ParseError pos msg :| []
