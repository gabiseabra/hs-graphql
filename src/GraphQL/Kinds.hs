{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , GADTs
  , StandaloneKindSignatures
  , FlexibleContexts
  , FlexibleInstances
  , TypeApplications
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , UndecidableInstances
  , TypeOperators
  , InstanceSigs
#-}

module GraphQL.Kinds
  ( GraphQLScalar(..)
  , GraphQLObject(..)
  , GraphQLInputObject(..)
  , Row
  ) where

import GraphQL.Internal
import GraphQL.Class
import GraphQL.IO.Input
import GraphQL.IO.Output

import GHC.Exts (Constraint)

import Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Aeson.Types ((<?>))
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Row (Rec)
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Text (Text)

type Row a = Rec.NativeRow a

data GraphQLScalar a where
  Scalar ::
    ( JSON.FromJSON a
    , JSON.ToJSON a
    ) => GraphQLScalar a

instance
  ( JSON.FromJSON a
  , JSON.ToJSON a
  ) => GraphQLTypeable GraphQLScalar a where
  typeOf = Scalar
instance GraphQLKind GraphQLScalar where type Kind GraphQLScalar = SCALAR
instance GraphQLInputKind GraphQLScalar where
  readInputType Scalar = liftJSONResult . JSON.fromJSON
instance GraphQLOutputKind m GraphQLScalar where
  mkResolver Scalar = Leaf

data GraphQLObject m r a where
  Object ::
    ( Rec.FromNative a
    , Rec.NativeRow a ~ r
    , Row.AllUniqueLabels r
    , Row.Forall r (GraphQLResolver m)
    , Row.FreeForall r
    ) => GraphQLObject m r a

instance
  ( Rec.FromNative a
  , Rec.NativeRow a ~ r
  , Row.AllUniqueLabels r
  , Row.Forall r (GraphQLResolver m)
  , Row.FreeForall r
  ) => GraphQLTypeable (GraphQLObject m r) a where
  typeOf = Object
instance GraphQLKind (GraphQLObject m r) where type Kind (GraphQLObject m r) = OBJECT
instance GraphQLOutputKind m (GraphQLObject m r) where
  mkResolver :: forall a. GraphQLObject m r a -> Resolver BRANCH a (Field m a)
  mkResolver Object
    = Branch
    $ eraseWithLabelsF @(GraphQLResolver m) @((->) a) @r (Field . fmap applyInput)
    $ accessors @a

data GraphQLInputObject r a where
  InputObject ::
    ( Rec.ToNative a
    , Rec.NativeRow a ~ r
    , Row.AllUniqueLabels r
    , Row.Forall r GraphQLInputType
    , Row.FreeForall r
    ) => GraphQLInputObject r a

instance
  ( Rec.ToNative a
  , Rec.NativeRow a ~ r
  , Row.AllUniqueLabels r
  , Row.Forall r GraphQLInputType
  , Row.FreeForall r
  ) => GraphQLTypeable (GraphQLInputObject r) a where typeOf = InputObject
instance GraphQLKind (GraphQLInputObject r) where type Kind (GraphQLInputObject r) = INPUT_OBJECT
instance GraphQLInputKind (GraphQLInputObject r) where readInputType InputObject = pure . Rec.toNative <=< readInputFields
