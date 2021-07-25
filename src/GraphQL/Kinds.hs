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
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Row (Rec, type (.!))
import qualified Data.Row as Row
import qualified Data.Row.Records as Rec
import Data.Text (Text)

type Row a = Rec.NativeRow a

data GraphQLScalar a where
  ScalarT ::
    ( JSON.FromJSON a
    , JSON.ToJSON a
    ) => GraphQLScalar a

instance
  ( JSON.FromJSON a
  , JSON.ToJSON a
  ) => GraphQLTypeable GraphQLScalar a where
  typeOf = ScalarT
instance GraphQLKind GraphQLScalar where type Kind GraphQLScalar = SCALAR
instance GraphQLInputKind GraphQLScalar where
  readInputType ScalarT = liftJSONResult . JSON.fromJSON
instance GraphQLOutputKind m GraphQLScalar where
  mkResolver ScalarT = Leaf

data GraphQLObject m a where
  ObjectT ::
    ( Rec.FromNative a
    , Row.AllUniqueLabels (Rec.NativeRow a)
    , Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
    , Row.FreeForall (Rec.NativeRow a)
    ) => GraphQLObject m a

instance
  ( Rec.FromNative a
  , Row.AllUniqueLabels (Rec.NativeRow a)
  , Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  , Row.FreeForall (Rec.NativeRow a)
  ) => GraphQLTypeable (GraphQLObject m) a where typeOf = ObjectT
instance GraphQLKind (GraphQLObject m) where type Kind (GraphQLObject m) = OBJECT
instance GraphQLOutputKind m (GraphQLObject m) where
  mkResolver :: forall a. GraphQLObject m a -> Resolver BRANCH (Field m) a
  mkResolver ObjectT
    = Branch
    $ Map.fromList
    $ eraseWithLabelsF
      @(GraphQLResolver m)
      @((->) a)
      @(Rec.NativeRow a)
      (Field . fmap applyInput)
    $ accessors @a

data GraphQLInputObject a where
  InputObjectT ::
    ( Rec.ToNative a
    , Row.AllUniqueLabels (Rec.NativeRow a)
    , Row.Forall (Rec.NativeRow a) GraphQLInputType
    , Row.FreeForall (Rec.NativeRow a)
    ) => GraphQLInputObject a

instance
  ( Rec.ToNative a
  , Row.AllUniqueLabels (Rec.NativeRow a)
  , Row.Forall (Rec.NativeRow a) GraphQLInputType
  , Row.FreeForall (Rec.NativeRow a)
  ) => GraphQLTypeable GraphQLInputObject a where typeOf = InputObjectT
instance GraphQLKind GraphQLInputObject where type Kind GraphQLInputObject = INPUT_OBJECT
instance GraphQLInputKind GraphQLInputObject where readInputType InputObjectT = pure . Rec.toNative <=< readInputFields

data GraphQLList t a where
  ListT ::
    ( Functor f
    , Traversable f
    , Applicative f
    , Monoid (f a)
    , JSON.ToJSON1 f
    ) => t a
      -> GraphQLList t (f a)

instance
  ( Functor f
  , Traversable f
  , Applicative f
  , Monoid (f a)
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (GraphQLList t) (f a) where typeOf = ListT typeOf
instance GraphQLKind t => GraphQLKind (GraphQLList t) where type Kind (GraphQLList t) = LIST (Kind t)
instance GraphQLInputKind t => GraphQLInputKind (GraphQLList t) where
  readInputType (ListT t) (JSON.Array array) = pure . foldMap pure =<< traverse (readInputType t) array
  readInputType _ _ = Left "Expected an array"
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (GraphQLList t) where
  mkResolver (ListT t) = Wrap (mkResolver @m @t t)

data GraphQLNullable t a where
  NullableT ::
    ( Functor f
    , Traversable f
    , Alternative f
    , JSON.ToJSON1 f
    ) => t a
      -> GraphQLNullable t (f a)

instance
  ( Functor f
  , Traversable f
  , Alternative f
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (GraphQLNullable t) (f a) where typeOf = NullableT typeOf
instance GraphQLKind t => GraphQLKind (GraphQLNullable t) where type Kind (GraphQLNullable t) = NULLABLE (Kind t)
instance GraphQLInputKind t => GraphQLInputKind (GraphQLNullable t) where
  readInputType (NullableT _) JSON.Null = pure empty
  readInputType (NullableT t) v = fmap pure (readInputType t v)
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (GraphQLNullable t) where
  mkResolver (NullableT t) = Wrap (mkResolver @m @t t)

data GraphQLVariant k r a where
  VarT ::
    ( Row.HasType k t r
    , GraphQLKind t
    ) => t a
      -> GraphQLVariant k r a

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
