{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}

module GraphQL.IO.Input
  ( E
  , VariableAssignment(..)
  , Variables
  , resolveVariables
  , GraphQLInputKind(..)
  , GraphQLInput(..)
  ) where

import GraphQL.Typeable
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

type E = Either Text

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
  readInputType :: t a -> JSON.Value -> E a

class IsInput a where
  toTypeRep :: proxy a -> TypeRep
instance ( GraphQLType a, InstanceOf t a ) => IsInput a where
  toTypeRep _ = TypeRep (typeOf @a)

-- | A GraphQL input is an object of input types (not a proper type)
class Row.FreeForall (InputFieldsOf a) => GraphQLInput a where
  type InputFieldsOf a :: Row *
  toInputFieldsList :: proxy a -> [InputField]
  -- default toInputFieldsList
  --   :: Row.ToNative a
  --   =>
  fromInputFields :: Rec (InputFieldsOf a) -> a

instance GraphQLInput () where
  type InputFieldsOf () = Row.Empty
  toInputFieldsList = const []
  fromInputFields = const ()
