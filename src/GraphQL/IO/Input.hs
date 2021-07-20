{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  , TypeApplications
  , ScopedTypeVariables
  , DefaultSignatures
  , TypeOperators
#-}

module GraphQL.IO.Input where

import GraphQL.Internal
import GraphQL.Class
import GraphQL.IO.Kinds

import Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Aeson.Types ((<?>))
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Row.Records (Rec, Row)
import qualified Data.Row.Records as Rec
import qualified Data.Row as Row
import Data.Text (Text)
import qualified Data.Text as Text

type Input = JSON.Object

data VariableAssignment
  = Var Text
  | StrVal Text
  | IntVal Int
  | NumVal Double
  | BoolVal Bool
  | EnumVal Text
  | ListVal [VariableAssignment]
  | ObjectVal [(Text, VariableAssignment)]
  deriving (Eq, Show)

type Variables = [(Text, VariableAssignment)]

resolveVariables :: Variables -> Input -> Input
resolveVariables vars input = Map.fromList (map (fmap enc) vars)
  where
    enc (StrVal v) = JSON.toJSON v
    enc (IntVal v) = JSON.toJSON v
    enc (NumVal v) = JSON.toJSON v
    enc (BoolVal v) = JSON.toJSON v
    enc (EnumVal v) = JSON.toJSON v
    enc (ListVal v) = JSON.toJSON (map enc v)
    enc (ObjectVal v) = JSON.Object (Map.fromList (map (fmap enc) v))
    enc (Var v) = fromMaybe JSON.Null (Map.lookup v input)

-- | A GraphQL type that is allowed in inputs
class
  ( GraphQLKind t
  , (Kind t) !>> IN
  ) => GraphQLInputKind (t :: * -> *) where
  readInputType :: MonadFail v => t a -> JSON.Value -> v a

class GraphQLInput a where
  readInput :: MonadFail v => Input -> v a
  default readInput
    :: MonadFail v
    => Rec.ToNative a
    => Rec.AllUniqueLabels (Rec.NativeRow a)
    => Rec.Forall (Rec.NativeRow a) GraphQLInputType
    => Input
    -> v a
  readInput = pure . Rec.toNative <=< readInputFields . JSON.Object

instance GraphQLInput () where readInput _ = pure ()

class
  ( GraphQLType a
  , GraphQLInputKind (KindOf a)
  ) => GraphQLInputType a where
  readInputType' :: MonadFail v => JSON.Value -> v a
instance
  ( GraphQLType a
  , GraphQLInputKind (KindOf a)
  ) => GraphQLInputType a where
  readInputType' = readInputType typeOf_

parseInputFields :: forall r
  .  Row.AllUniqueLabels r
  => Row.Forall r GraphQLInputType
  => JSON.Value
  -> JSON.Parser (Rec r)
parseInputFields (JSON.Object obj) = Rec.fromLabelsA @GraphQLInputType @JSON.Parser @r readField
  where
    readField :: forall l a. (Row.KnownSymbol l, GraphQLInputType a) => Row.Label l -> JSON.Parser a
    readField lbl =
      let
        key = Text.pack (show lbl)
        val = fromMaybe JSON.Null $ Map.lookup key obj
      in readInputType' val <?> JSON.Key key
parseInputFields actual = JSON.typeMismatch expected actual
  where
    lbls = Rec.labels @r @GraphQLInputType
    expected = "{ " <> List.intercalate ", " lbls <> " }"

readInputFields :: forall r v
  .  MonadFail v
  => Row.AllUniqueLabels r
  => Row.Forall r GraphQLInputType
  => JSON.Value
  -> v (Rec r)
readInputFields = liftJSONResult . JSON.parse parseInputFields
