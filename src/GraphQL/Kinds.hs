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
  ( SCALAR
  , OBJECT
  , INPUT_OBJECT
  , LIST
  , NULLABLE
  , type (.@)
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

data SCALAR a where
  ScalarT ::
    ( JSON.FromJSON a
    , JSON.ToJSON a
    ) => SCALAR a

instance
  ( JSON.FromJSON a
  , JSON.ToJSON a
  ) => GraphQLTypeable SCALAR a where
  typeOf = ScalarT
instance GraphQLKind SCALAR where type KIND SCALAR = GQL_SCALAR
instance GraphQLInputKind SCALAR where
  readInputType ScalarT = liftJSONResult . JSON.fromJSON
instance GraphQLOutputKind m SCALAR where
  mkResolver ScalarT = Leaf

data OBJECT m a where
  ObjectT ::
    ( Rec.FromNative a
    , Row.AllUniqueLabels (Rec.NativeRow a)
    , Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
    , Row.FreeForall (Rec.NativeRow a)
    ) => OBJECT m a

instance
  ( Rec.FromNative a
  , Row.AllUniqueLabels (Rec.NativeRow a)
  , Row.Forall (Rec.NativeRow a) (GraphQLResolver m)
  , Row.FreeForall (Rec.NativeRow a)
  ) => GraphQLTypeable (OBJECT m) a where typeOf = ObjectT
instance GraphQLKind (OBJECT m) where type KIND (OBJECT m) = GQL_OBJECT
instance GraphQLOutputKind m (OBJECT m) where
  mkResolver :: forall a. OBJECT m a -> Resolver BRANCH (Field m) a
  mkResolver ObjectT
    = Branch
    $ Map.fromList
    $ eraseWithLabelsF
      @(GraphQLResolver m)
      @((->) a)
      @(Rec.NativeRow a)
      (Field . fmap applyInput)
    $ accessors @a

data INPUT_OBJECT a where
  InputObjectT ::
    ( Rec.ToNative a
    , Row.AllUniqueLabels (Rec.NativeRow a)
    , Row.Forall (Rec.NativeRow a) GraphQLInputType
    , Row.FreeForall (Rec.NativeRow a)
    ) => INPUT_OBJECT a

instance
  ( Rec.ToNative a
  , Row.AllUniqueLabels (Rec.NativeRow a)
  , Row.Forall (Rec.NativeRow a) GraphQLInputType
  , Row.FreeForall (Rec.NativeRow a)
  ) => GraphQLTypeable INPUT_OBJECT a where typeOf = InputObjectT
instance GraphQLKind INPUT_OBJECT where type KIND INPUT_OBJECT = GQL_INPUT_OBJECT
instance GraphQLInputKind INPUT_OBJECT where readInputType InputObjectT = pure . Rec.toNative <=< readInputFields

data LIST t a where
  ListT ::
    ( Functor f
    , Traversable f
    , Applicative f
    , Monoid (f a)
    , JSON.ToJSON1 f
    ) => t a
      -> LIST t (f a)

instance
  ( Functor f
  , Traversable f
  , Applicative f
  , Monoid (f a)
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (LIST t) (f a) where typeOf = ListT typeOf
instance GraphQLKind t => GraphQLKind (LIST t) where type KIND (LIST t) = GQL_LIST (KIND t)
instance GraphQLInputKind t => GraphQLInputKind (LIST t) where
  readInputType (ListT t) (JSON.Array array) = pure . foldMap pure =<< traverse (readInputType t) array
  readInputType _ _ = Left "Expected an array"
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (LIST t) where
  mkResolver (ListT t) = Wrap (mkResolver @m @t t)

data NULLABLE t a where
  NullableT ::
    ( Functor f
    , Traversable f
    , Alternative f
    , JSON.ToJSON1 f
    ) => t a
      -> NULLABLE t (f a)

instance
  ( Functor f
  , Traversable f
  , Alternative f
  , JSON.ToJSON1 f
  , GraphQLTypeable t a
  ) => GraphQLTypeable (NULLABLE t) (f a) where typeOf = NullableT typeOf
instance GraphQLKind t => GraphQLKind (NULLABLE t) where type KIND (NULLABLE t) = GQL_NULLABLE (KIND t)
instance GraphQLInputKind t => GraphQLInputKind (NULLABLE t) where
  readInputType (NullableT _) JSON.Null = pure empty
  readInputType (NullableT t) v = fmap pure (readInputType t v)
instance
  ( Applicative m
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (NULLABLE t) where
  mkResolver (NullableT t) = Wrap (mkResolver @m @t t)

data VAR k r a where
  VarT ::
    ( Row.HasType k t r
    , GraphQLKind t
    ) => t a
      -> VAR k r a

type (.@) :: Symbol -> Row.Row (* -> *) -> (* -> *)
type (.@) k r = VAR k r

instance
  ( Row.HasType k t r
  , GraphQLKind t
  , GraphQLTypeable t a
  ) => GraphQLTypeable (VAR k r) a where typeOf = VarT (typeOf @t @a)
instance
  ( Row.HasType k t r
  , GraphQLKind t
  ) => GraphQLKind (VAR k r) where type KIND (VAR k r) = KIND (r .! k)
instance
  ( Row.HasType k t r
  , GraphQLInputKind t
  ) => GraphQLInputKind (VAR k r) where readInputType (VarT t) = readInputType t
instance
  ( Row.HasType k t r
  , GraphQLOutputKind m t
  ) => GraphQLOutputKind m (VAR k r) where mkResolver (VarT t) = mkResolver t
