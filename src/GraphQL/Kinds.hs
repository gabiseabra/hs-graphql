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
  , QuantifiedConstraints
#-}

module GraphQL.Kinds
  ( GraphQLScalar
  , GraphQLObject
  , GraphQLInputObject
  , GraphQLList
  , GraphQLNullable
  , type (.@)
  , Row
  ) where

import GraphQL.Internal
import GraphQL.Class
import GraphQL.IO.Input
import GraphQL.IO.Output

import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol)

import Control.Applicative (Alternative(..))
import Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Aeson.Types ((<?>))
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Row (Rec, type (.!))
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
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
  ) => GraphQLTypeable (GraphQLObject m r) a where typeOf = Object
instance GraphQLKind (GraphQLObject m r) where type Kind (GraphQLObject m r) = OBJECT
instance GraphQLOutputKind m (GraphQLObject m r) where
  mkResolver :: forall a. GraphQLObject m r a -> Resolver BRANCH (Field m) a
  mkResolver Object
    = Branch
    $ Map.fromList
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

data GraphQLList t f a where
  List ::
    ( Functor f
    , Traversable f
    , Applicative f
    , Monoid (f a)
    , JSON.ToJSON1 f
    ) => t a
      -> GraphQLList t f (f a)

instance
  ( Functor f
  , Traversable f
  , Applicative f
  , Monoid (f a)
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (GraphQLList t f) (f a) where typeOf = List typeOf
instance GraphQLKind t => GraphQLKind (GraphQLList t f) where type Kind (GraphQLList t f) = LIST (Kind t)
instance GraphQLInputKind t => GraphQLInputKind (GraphQLList t f) where
  readInputType (List t) (JSON.Array array) = pure . foldMap pure =<< traverse (readInputType t) array
  readInputType _ _ = Left "Expected an array"
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (GraphQLList t f) where
  mkResolver (List t) = Wrap (mkResolver @m @t t)

data GraphQLNullable t f a where
  Nullable ::
    ( Functor f
    , Traversable f
    , Alternative f
    , JSON.ToJSON1 f
    ) => t a
      -> GraphQLNullable t f (f a)

instance
  ( Functor f
  , Traversable f
  , Alternative f
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (GraphQLNullable t f) (f a) where typeOf = Nullable typeOf
instance GraphQLKind t => GraphQLKind (GraphQLNullable t f) where type Kind (GraphQLNullable t f) = NULLABLE (Kind t)
instance GraphQLInputKind t => GraphQLInputKind (GraphQLNullable t f) where
  readInputType (Nullable _) JSON.Null = pure empty
  readInputType (Nullable t) v = fmap pure (readInputType t v)
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (GraphQLNullable t f) where
  mkResolver (Nullable t) = Wrap (mkResolver @m @t t)

data GraphQLVariant k r a where
  VarT ::
    ( Row.HasType k t r
    , GraphQLKind t
    ) => t a -> GraphQLVariant k r a

type (.@) :: Symbol -> Row.Row (* -> *) -> (* -> *)
type (.@) k r = GraphQLVariant k r

instance
  ( Row.HasType k t r
  , GraphQLKind t
  , GraphQLTypeable t a
  ) => GraphQLTypeable (GraphQLVariant k r) a where typeOf = VarT (typeOf @t @a)
instance
  ( Row.HasType k t r
  , GraphQLKind t
  ) => GraphQLKind (GraphQLVariant k r) where type Kind (GraphQLVariant k r) = Kind (r .! k)
instance
  ( Row.HasType k t r
  , GraphQLInputKind t
  ) => GraphQLInputKind (GraphQLVariant k r) where readInputType (VarT t) = readInputType t
instance
  ( Row.HasType k t r
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (GraphQLVariant k r) where mkResolver (VarT t) = mkResolver t
