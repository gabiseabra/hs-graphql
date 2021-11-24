{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveAnyClass
  , DeriveGeneric
  , DeriveDataTypeable
  , TypeOperators
  , OverloadedStrings
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , UndecidableInstances
#-}

module GraphQL.AST.Document where

import GraphQL.TypeSystem.Main (OperationType(..))
import GraphQL.Response (Pos)
import GraphQL.Internal

import GHC.Generics (Generic1)

import Control.Comonad.Cofree (Cofree(..), hoistCofree)
import qualified Data.Aeson as JSON
import Data.Void (Void)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (All(..))
import Data.Vector (Vector)
import Data.Data (Data)
import Data.Data.Lens (biplate)
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Fix (Fix(..))
import Data.Functor.Base (TreeF)
import Data.Functor.Foldable (cata)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Classes (Show1(..), Eq1(..), showsUnaryWith, showsPrec1, eq1)
import Data.Functor.Sum (Sum(..))
import Control.Lens (Lens, Lens', Traversal', Traversal, lens, (<&>))
import Control.Lens.Traversal (IndexedTraversal)
import Control.Lens.Indexed (Indexable(..), itraverse)
import Control.Arrow ((&&&))

-- * Value node

data ConstValueF r
  = NullVal
  | StrVal Text
  | IntVal Int
  | DoubleVal Double
  | BoolVal Bool
  | EnumVal Text
  | ListVal (Vector r)
  | ObjectVal (HashMap Name r)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic1)

type ConstValue = Att ConstValueF

type Value = Att (Const Name :+: ConstValueF)

instance Eq1 ConstValueF where
  liftEq _ NullVal        NullVal        = True
  liftEq _ (StrVal    a ) (StrVal     b) = a == b
  liftEq _ (IntVal    a ) (IntVal     b) = a == b
  liftEq _ (DoubleVal a ) (DoubleVal  b) = a == b
  liftEq _ (BoolVal   a ) (BoolVal    b) = a == b
  liftEq _ (EnumVal   a ) (EnumVal    b) = a == b
  liftEq f (ListVal   as) (ListVal   bs) = liftEq f as bs
  liftEq f (ObjectVal as) (ObjectVal bs) = liftEq f as bs
  liftEq _ _ _                           = False

instance Show1 ConstValueF where
  liftShowsPrec _  _  _ NullVal        = (<>) "NullVal"
  liftShowsPrec _  _  _ (StrVal     a) = (<>) $ "StrVal " <> show a
  liftShowsPrec _  _  _ (IntVal     a) = (<>) $ "IntVal " <> show a
  liftShowsPrec _  _  _ (DoubleVal  a) = (<>) $ "DoubleVal " <> show a
  liftShowsPrec _  _  _ (BoolVal    a) = (<>) $ "BoolVal " <> show a
  liftShowsPrec _  _  _ (EnumVal    a) = (<>) $ "EnumVal " <> show a
  liftShowsPrec sp sl d (ListVal   as) = showsUnaryWith (liftShowsPrec sp sl) "ListVal" d as
  liftShowsPrec sp sl d (ObjectVal as) = showsUnaryWith (liftShowsPrec sp sl) "ObjectVal" d as

instance JSON.ToJSON a => JSON.ToJSON (ConstValueF a) where
  toJSON NullVal       = JSON.Null
  toJSON (StrVal a)    = JSON.toJSON a
  toJSON (IntVal a)    = JSON.toJSON a
  toJSON (DoubleVal a) = JSON.toJSON a
  toJSON (BoolVal a)   = JSON.toJSON a
  toJSON (EnumVal a)   = JSON.toJSON a
  toJSON (ListVal r)   = JSON.toJSON $ fmap JSON.toJSON r
  toJSON (ObjectVal r) = JSON.Object $ fmap JSON.toJSON r

instance JSON.ToJSON1 ConstValueF where
  liftToJSON _ _ NullVal       = JSON.Null
  liftToJSON _ _ (StrVal a)    = JSON.toJSON a
  liftToJSON _ _ (IntVal a)    = JSON.toJSON a
  liftToJSON _ _ (DoubleVal a) = JSON.toJSON a
  liftToJSON _ _ (BoolVal a)   = JSON.toJSON a
  liftToJSON _ _ (EnumVal a)   = JSON.toJSON a
  liftToJSON f _ (ListVal r)   = JSON.toJSON $ fmap f r
  liftToJSON f _ (ObjectVal r) = JSON.Object $ fmap f r

-- * Variable node

data Variable
  = Variable
    { varPos :: Pos
    , varDefinition :: TypeDefinition
    , varValue :: Maybe ConstValue
    } deriving (Eq, Show)

-- * Type definition node

data TypeDefinitionF r
  = ListType r
  | NonNullType r
  | NamedType Name
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq1 TypeDefinitionF where
  liftEq f (ListType a) (ListType b) = f a b
  liftEq f (NonNullType a) (NonNullType b) = f a b
  liftEq _ (NamedType a) (NamedType b) = a == b
  liftEq _ _ _ = False

instance Show1 TypeDefinitionF where
  liftShowsPrec sp sl d (ListType a) = showsUnaryWith sp "ListType" d a
  liftShowsPrec sp sl d (NonNullType a) = showsUnaryWith sp "NonNullType" d a
  liftShowsPrec _  _  _ (NamedType ty) = (<>) (Text.unpack ty)

type TypeDefinition = Fix TypeDefinitionF

showTypeDefinition :: TypeDefinition -> Text
showTypeDefinition = cata alg
  where
    alg (ListType ty) = "[" <> ty <> "]"
    alg (NonNullType ty) = ty <> "!"
    alg (NamedType ty) = ty

checkTypeDefinition :: TypeDefinition -> ConstValue -> Bool
checkTypeDefinition (Fix (NonNullType ty)) (_:<NullVal) = False
checkTypeDefinition (Fix (NonNullType ty)) val = checkTypeDefinition ty val
checkTypeDefinition (Fix (ListType ty)) (_:<ListVal as) = getAll . foldMap (All . checkTypeDefinition ty) $ as
checkTypeDefinition (Fix (ListType ty)) _ = False
checkTypeDefinition _ _ = True

isNullable :: TypeDefinition -> Bool
isNullable (Fix (NonNullType _)) = False
isNullable _ = True

-- * Selection nodes

data Field a
  = Field
    { fieldType :: Maybe Name
    , fieldAlias :: Maybe Name
    , fieldName :: Name
    , fieldArgs :: HashMap Name a
    } deriving (Eq, Show, Data, Functor, Foldable, Traversable)

instance Eq1 Field where
  liftEq f (Field ty alias name args) (Field ty' alias' name' args')
    =  ty == ty'
    && alias == alias'
    && name == name'
    && liftEq f args args'

instance Show1 Field where
  liftShowsPrec sp sl d (Field ty alias name args)
    = showsUnaryWith
      (liftShowsPrec sp sl)
      ("Field " <> show ty <> " " <> show alias <> " " <> show name)
      d args

data SelectionF a r
  = Node a [r]
  | FragmentSpread Name
  | InlineFragment Name (NonEmpty r)
  deriving (Eq, Show, Data, Functor, Foldable, Traversable)

type Selection a = Att (SelectionF a)

data Ix a
  = NodeIx a Int
  | FragmentIx Name Int
  deriving (Eq, Show)

instance Bifunctor SelectionF where
  bimap f g (Node a r)           = Node (f a) (fmap g r)
  bimap f g (FragmentSpread a)   = FragmentSpread a
  bimap f g (InlineFragment a r) = InlineFragment a (fmap g r)

instance Bifoldable SelectionF where
  bifoldMap f g (Node a r)           = f a `mappend` foldMap g r
  bifoldMap f g (FragmentSpread a)   = mempty
  bifoldMap f g (InlineFragment a r) = foldMap g r

instance Bitraversable SelectionF where
  bitraverse f g (Node a r)           = Node <$> f a <*> traverse g r
  bitraverse f g (FragmentSpread a)   = pure $ FragmentSpread a
  bitraverse f g (InlineFragment a r) = InlineFragment a <$> traverse g r

instance Eq a => Eq1 (SelectionF a) where
  liftEq f (Node a l)           (Node b r)           = a == b && liftEq f l r
  liftEq f (FragmentSpread a)   (FragmentSpread b)   = a == b
  liftEq f (InlineFragment a l) (InlineFragment b r) = a == b && liftEq f l r
  liftEq _ _ _                           = False

instance Show a => Show1 (SelectionF a) where
  liftShowsPrec sp sl d (Node a r)           = showsUnaryWith (liftShowsPrec sp sl) ("Node " <> show a) d r
  liftShowsPrec sp sl d (FragmentSpread a)   = (<>) $ "FragmentSpread " <> show a
  liftShowsPrec sp sl d (InlineFragment a r) = showsUnaryWith (liftShowsPrec sp sl) ("InlineFragment " <> show a) d r

_nodes :: IndexedTraversal [Ix a] (Cofree (SelectionF a) x) (Cofree (SelectionF b) x) a b
_nodes = flip go []
  where
    go f path (x:<y) = (x:<) <$> case y of
      Node a xs -> Node <$> indexed f path a <*> itraverse (go f . (:path) . NodeIx a) xs
      InlineFragment a xs -> InlineFragment a <$> itraverse (go f . (:path) . FragmentIx a) xs
      FragmentSpread a -> pure $ FragmentSpread a

ix :: SelectionF a r -> SelectionF a ()
ix = second (const ())

-- * Root nodes

data Fragment a
  = Fragment
    { fragPos :: Pos
    , fragName :: Name
    , fragTypename :: Name
    , fragSelection :: NonEmpty a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

_fragSelection :: Lens (Fragment a) (Fragment b) (NonEmpty a) (NonEmpty b)
_fragSelection f frag = f (fragSelection frag) <&> \a -> frag { fragSelection = a }

instance Eq1 Fragment where
  liftEq f (Fragment pos name ty as) (Fragment pos' name' ty' bs)
    =  pos == pos'
    && name == name'
    && ty == ty'
    && liftEq f as bs

instance Show1 Fragment where
  liftShowsPrec sp sl d (Fragment pos name ty as)
    = showsUnaryWith
      (liftShowsPrec sp sl)
      ("Fragment " <> " " <> show pos <> "  " <> show name <> " " <> show ty)
      d as

data Operation a
  = Query        Pos (Maybe Name) (HashMap Name Variable) (NonEmpty a)
  | Mutation     Pos (Maybe Name) (HashMap Name Variable) (NonEmpty a)
  | Subscription Pos (Maybe Name) (HashMap Name Variable) a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq1 Operation where
  liftEq f (Query        pos name vars as) (Query        pos' name' vars' bs) = pos == pos' && name == name' && vars == vars && liftEq f as bs
  liftEq f (Mutation     pos name vars as) (Mutation     pos' name' vars' bs) = pos == pos' && name == name' && vars == vars && liftEq f as bs
  liftEq f (Subscription pos name vars a ) (Subscription pos' name' vars' b ) = pos == pos' && name == name' && vars == vars && liftEq f (Identity a) (Identity b)

instance Show1 Operation where
  liftShowsPrec sp sl d (Query pos name vars as)
    = showsUnaryWith (liftShowsPrec sp sl) ("Query " <>  show pos <> " " <> show name <> " " <> show vars) d as
  liftShowsPrec sp sl d (Mutation pos name vars as)
    = showsUnaryWith (liftShowsPrec sp sl) ("Mutation " <>  show pos <> " " <> show name <> " " <> show vars) d as
  liftShowsPrec sp sl d (Subscription pos name vars a)
    = showsUnaryWith sp ("Subscription " <> show pos <> " " <> show name <> " " <> show vars) d a

opType :: Operation a -> OperationType
opType Query {} = QUERY
opType Mutation {} = MUTATION
opType Subscription {} = SUBSCRIPTION

opPos :: Operation a -> Pos
opPos (Query        pos _ _ _) = pos
opPos (Mutation     pos _ _ _) = pos
opPos (Subscription pos _ _ _) = pos

opName :: Operation a -> Maybe Name
opName (Query        _ name _ _) = name
opName (Mutation     _ name _ _) = name
opName (Subscription _ name _ _) = name

_opVariables :: Lens' (Operation a) (HashMap Name Variable)
_opVariables f op = f (get op) <&> set op
  where
    get (Query        _ _ vars _) = vars
    get (Mutation     _ _ vars _) = vars
    get (Subscription _ _ vars _) = vars

    set (Query        pos name _ as) vars = Query        pos name vars as
    set (Mutation     pos name _ as) vars = Mutation     pos name vars as
    set (Subscription pos name _ as) vars = Subscription pos name vars as

_opSelection :: Lens (Operation a) (Operation b) (NonEmpty a) (NonEmpty b)
_opSelection f op = f (get op) <&> set op
  where
    get (Query        _ _ _ as) = as
    get (Mutation     _ _ _ as) = as
    get (Subscription _ _ _ a ) = a:|[]

    set (Query        pos name vars _) as     = Query        pos name vars as
    set (Mutation     pos name vars _) as     = Mutation     pos name vars as
    set (Subscription pos name vars _) (a:|_) = Subscription pos name vars a

-- * Document

data Document a
  = Document
    { fragments :: HashMap Name (Fragment a)
    , operations :: (Identity :+: HashMap Name) (Operation a)
    } deriving (Eq, Show, Functor, Foldable, Traversable)

_docFragments :: Lens' (Document a) (HashMap Name (Fragment a))
_docFragments f doc = f (fragments doc) <&> \a -> doc { fragments = a }

_docOperations :: Lens' (Document a) ((Identity :+: HashMap Name) (Operation a))
_docOperations f doc = f (operations doc) <&> \a -> doc { operations = a }

_docSelections :: Traversal' (Document a) a
_docSelections = traverse

type Name = Text

type Tree a = Att (TreeF a)

type Att f = Cofree f Pos

type ExecutableOperation = Operation (Tree (Field JSON.Value))

type a :+: b = Sum a b
