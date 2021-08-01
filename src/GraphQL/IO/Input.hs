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
#-}

module GraphQL.IO.Input where

import GraphQL.Class
import GraphQL.Internal

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
  , (KIND t) !>> IN
  ) => GraphQLInputKind (t :: * -> *) where
  readInputType :: t a -> JSON.Value -> V a

class GraphQLInput a where
  inputFields :: InputFields a
  default inputFields
    :: Rec.ToNative a
    => Row.AllUniqueLabels (Rec.NativeRow a)
    => Rec.Forall (Rec.NativeRow a) GraphQLInputType
    => InputFields a
  inputFields = InputFields Rec.toNative

data InputFields a where
  InputFields ::
    ( Row.Forall r GraphQLInputType
    , Row.AllUniqueLabels r
    ) => (Rec r -> a)
      -> InputFields a

instance GraphQLInput () where inputFields = InputFields (\(_ :: Rec Row.Empty) -> ())

class
  ( GraphQLType a
  , GraphQLInputKind (KindOf a)
  ) => GraphQLInputType a where
  readInputType' :: JSON.Value -> V a
instance
  ( GraphQLType a
  , GraphQLInputKind (KindOf a)
  ) => GraphQLInputType a where
  readInputType' = readInputType typeOf_

readInputFields :: forall r
  .  Row.AllUniqueLabels r
  => Row.Forall r GraphQLInputType
  => JSON.Value
  -> V (Rec r)
readInputFields (JSON.Object obj) = Rec.fromLabelsA @GraphQLInputType readField
  where
    readField :: forall l a. (Row.KnownSymbol l, GraphQLInputType a) => Row.Label l -> V a
    readField lbl =
      let
        key = Text.pack (show lbl)
        val = fromMaybe JSON.Null $ Map.lookup key obj
      in readInputType' val
readInputFields actual = Left "expected an object"

readInput :: forall a. GraphQLInput a => Input -> V a
readInput = case inputFields @a of InputFields f -> pure . f <=< readInputFields . JSON.Object
