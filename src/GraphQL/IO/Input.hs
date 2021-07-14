{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module GraphQL.IO.Input
  ( VariableAssignment(..)
  , Variables
  , resolveVariables
  , GraphQLInputKind(..)
  , GraphQLInput(..)
  , IsInput(..)
  , ToInputFields
  ) where

import GraphQL.Internal (mapRow)
import GraphQL.Class
import GraphQL.IO.Kinds

import Control.Monad.Except (MonadError(..))
import Data.Aeson (FromJSON(..), Object, Value(..), Result(..))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Row.Records (Rec, Row)
import qualified Data.Row.Records as Rec
import qualified Data.Row as Row
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
    enc (ObjectVal v) = Object (Map.fromList (map (fmap enc) v))
    enc (Var v) = fromMaybe Null (Map.lookup v input)

-- | A GraphQL type that is allowed in inputs
class
  ( GraphQLKind t
  , (Kind t) !>> IN
  ) => GraphQLInputKind (t :: * -> *) where
  readInputType :: t a -> JSON.Value -> JSON.Result a

-- | A GraphQL input is an object of input types (not a proper type)
class Row.Forall (InputFieldsOf a) IsInput => GraphQLInput a where
  type InputFieldsOf a :: Row *
  toInputFieldsList :: proxy a -> [InputField]
  default toInputFieldsList
    :: Rec.AllUniqueLabels (InputFieldsOf a)
    => Rec.ToNative a
    => InputFieldsOf a ~ Rec.NativeRow a
    => proxy a
    -> [InputField]
  toInputFieldsList _ = mapRow @IsInput @(InputFieldsOf a) mkInputField
  fromInputFields :: Rec (InputFieldsOf a) -> a
  default fromInputFields
    :: Rec.AllUniqueLabels (InputFieldsOf a)
    => Rec.ToNative a
    => InputFieldsOf a ~ Rec.NativeRow a
    => Rec (InputFieldsOf a)
    -> a
  fromInputFields = Rec.toNative

instance GraphQLInput () where
  type InputFieldsOf () = Row.Empty
  toInputFieldsList = const []
  fromInputFields = const ()

class IsInput a where
  readInput :: JSON.Value -> JSON.Result a
  mkInputField :: Row.KnownSymbol l => Row.Label l -> proxy a -> InputField
instance
  ( GraphQLType a
  , InstanceOf t a
  , GraphQLInputKind t
  ) => IsInput a where
  readInput = readInputType (typeOf @a)
  mkInputField l _
    = InputField
      { name = Text.pack (show l)
      , typeRep = TypeRep (typeOf @a)
      }

type ToInputFields a = Rec.NativeRow a
