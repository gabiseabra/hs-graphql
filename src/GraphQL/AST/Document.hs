{-# LANGUAGE
    TypeFamilies
  , GADTs
  , DataKinds
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveAnyClass
#-}

module GraphQL.AST.Document where

import GraphQL.TypeSystem.Main (OperationType(..))
import GraphQL.Response (Pos)

import Control.Comonad.Cofree (Cofree, ComonadCofree(..))
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Fix (Fix)
import Data.Functor.Base (TreeF)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Functor.Identity (Identity(..))
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Functor.Classes (Show1(..), Eq1(..), showsUnaryWith, showsPrec1, eq1)

type Name = Text

data ConstValueF r
  = NullVal
  | StrVal Text
  | IntVal Int
  | DoubleVal Double
  | BoolVal Bool
  | EnumVal Text
  | ListVal (Vector r)
  | ObjectVal (HashMap Name r)
  deriving (Eq, Show, Functor, Foldable, Traversable)

type ConstValue = Cofree ConstValueF Pos

showConstVal :: ConstValueF ConstValue -> String
showConstVal NullVal       = "null"
showConstVal (StrVal a)    = show a
showConstVal (IntVal a)    = show a
showConstVal (DoubleVal a) = show a
showConstVal (BoolVal a)   = show a
showConstVal (EnumVal a)   = show a
showConstVal (ListVal r)   = "[" <> List.intercalate ", " as <> "]"
  where as = foldMap (pure . showConstVal . unwrap) r
showConstVal (ObjectVal r) = "{" <> List.intercalate ", " kv <> "}"
  where kv = HashMap.foldMapWithKey (\k v -> [show k <> ": " <> showConstVal (unwrap v)]) r

constValToJSON :: ConstValueF ConstValue -> JSON.Value
constValToJSON NullVal       = JSON.Null
constValToJSON (StrVal a)    = JSON.toJSON a
constValToJSON (IntVal a)    = JSON.toJSON a
constValToJSON (DoubleVal a) = JSON.toJSON a
constValToJSON (BoolVal a)   = JSON.toJSON a
constValToJSON (EnumVal a)   = JSON.toJSON a
constValToJSON (ListVal r)   = JSON.toJSON $ fmap (constValToJSON . unwrap) r
constValToJSON (ObjectVal r) = JSON.Object $ fmap (constValToJSON . unwrap) r

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
  liftShowsPrec _  _  _ NullVal        = (<>) $ "NullVal"
  liftShowsPrec _  _  _ (StrVal     a) = (<>) $ "StrVal " <> show a
  liftShowsPrec _  _  _ (IntVal     a) = (<>) $ "IntVal " <> show a
  liftShowsPrec _  _  _ (DoubleVal  a) = (<>) $ "DoubleVal " <> show a
  liftShowsPrec _  _  _ (BoolVal    a) = (<>) $ "BoolVal " <> show a
  liftShowsPrec _  _  _ (EnumVal    a) = (<>) $ "EnumVal " <> show a
  liftShowsPrec sp sl d (ListVal   as) = showsUnaryWith (liftShowsPrec sp sl) "ListVal" d as
  liftShowsPrec sp sl d (ObjectVal as) = showsUnaryWith (liftShowsPrec sp sl) "ObjectVal" d as

data ValueF a r
  = Var a
  | Val (ConstValueF r)
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Value a = Cofree (ValueF a) Pos

instance Bifunctor ValueF where
  bimap f _ (Var a) = Var $ f a
  bimap _ g (Val a) = Val $ fmap g a

instance Bifoldable ValueF where
  bifoldMap f _ (Var v) = f v
  bifoldMap _ g (Val v) = foldMap g v

instance Bitraversable ValueF where
  bitraverse f _ (Var a) = Var <$> f a
  bitraverse _ g (Val a) = Val <$> traverse g a

instance Eq a => Eq1 (ValueF a) where
  liftEq _ (Var a) (Var b) = a == b
  liftEq f (Val a) (Val b) = liftEq f a b
  liftEq _ _ _             = False

instance Show a => Show1 (ValueF a) where
  liftShowsPrec sp sl d (Var v) = (<>) $ "Var " <> show v
  liftShowsPrec sp sl d (Val v) = showsUnaryWith (liftShowsPrec sp sl) "Val " d v

data TypeDefinition
  = ListType TypeDefinition
  | NonNullType TypeDefinition
  | NamedType Name
  deriving (Eq)

instance Show TypeDefinition where
  show (ListType ty) = "[" <> show ty <> "]"
  show (NonNullType ty) = show ty <> "!"
  show (NamedType ty) = Text.unpack ty

data FieldF a
  = Field
    { fieldType :: Maybe Name
    , fieldAlias :: Maybe Name
    , fieldName :: Name
    , fieldArgs :: HashMap Name a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq1 FieldF where
  liftEq f (Field ty alias name args) (Field ty' alias' name' args')
    =  ty == ty'
    && alias == alias'
    && name == name'
    && liftEq f args args'

instance Show1 FieldF where
  liftShowsPrec sp sl d (Field ty alias name args)
    = showsUnaryWith
      (liftShowsPrec sp sl)
      ("Field " <> show ty <> " " <> show alias <> " " <> show name)
      d args

data SelectionNodeF a r
  = Node a [r]
  | FragmentSpread Name
  | InlineFragment Name (NonEmpty r)
  deriving (Eq, Show, Functor, Foldable, Traversable)

type SelectionNode a = Cofree (SelectionNodeF a) Pos

instance Bifunctor SelectionNodeF where
  bimap f g (Node a r)           = Node (f a) (fmap g r)
  bimap f g (FragmentSpread a)   = FragmentSpread a
  bimap f g (InlineFragment a r) = InlineFragment a (fmap g r)

instance Bifoldable SelectionNodeF where
  bifoldMap f g (Node a r)           = f a `mappend` (foldMap g r)
  bifoldMap f g (FragmentSpread a)   = mempty
  bifoldMap f g (InlineFragment a r) = foldMap g r

instance Bitraversable SelectionNodeF where
  bitraverse f g (Node a r)           = Node <$> f a <*> traverse g r
  bitraverse f g (FragmentSpread a)   = pure $ FragmentSpread a
  bitraverse f g (InlineFragment a r) = InlineFragment a <$> traverse g r

instance Eq a => Eq1 (SelectionNodeF a) where
  liftEq f (Node a l)           (Node b r)           = a == b && liftEq f l r
  liftEq f (FragmentSpread a)   (FragmentSpread b)   = a == b
  liftEq f (InlineFragment a l) (InlineFragment b r) = a == b && liftEq f l r
  liftEq _ _ _                           = False

instance Show a => Show1 (SelectionNodeF a) where
  liftShowsPrec sp sl d (Node a r)           = showsUnaryWith (liftShowsPrec sp sl) ("Node " <> show a) d r
  liftShowsPrec sp sl d (FragmentSpread a)   = (<>) $ "FragmentSpread " <> show a
  liftShowsPrec sp sl d (InlineFragment a r) = showsUnaryWith (liftShowsPrec sp sl) ("InlineFragment " <> show a) d r

data FragmentF a
  = Fragment
    { fragPos ::Pos
    , fragTypename :: Name
    , fragSelection :: NonEmpty a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq1 FragmentF where
  liftEq f (Fragment pos ty as) (Fragment pos' ty' bs)
    =  pos == pos'
    && ty == ty'
    && liftEq f as bs

instance Show1 FragmentF where
  liftShowsPrec sp sl d (Fragment pos ty as)
    = showsUnaryWith
      (liftShowsPrec sp sl)
      ("Fragment " <> show pos <> " " <> show ty)
      d as

data DocumentF a
  = Query        (Maybe Name) (NonEmpty a)
  | Mutation     (Maybe Name) (NonEmpty a)
  | Subscription (Maybe Name) a
  deriving (Eq, Show, Functor, Foldable, Traversable)

opType :: DocumentF a -> OperationType
opType (Query        _ _) = QUERY
opType (Mutation     _ _) = MUTATION
opType (Subscription _ _) = SUBSCRIPTION

opName :: DocumentF a -> Maybe Name
opName (Query        name _) = name
opName (Mutation     name _) = name
opName (Subscription name _) = name

opSelection :: DocumentF a -> NonEmpty a
opSelection (Query        _ as) = as
opSelection (Mutation     _ as) = as
opSelection (Subscription _ a ) = a:|[]

instance Eq1 DocumentF where
  liftEq f (Query        name as) (Query        name' bs) = name == name' && liftEq f as bs
  liftEq f (Mutation     name as) (Mutation     name' bs) = name == name' && liftEq f as bs
  liftEq f (Subscription name a ) (Subscription name' b ) = name == name' && liftEq f (Identity a) (Identity b)

instance Show1 DocumentF where
  liftShowsPrec sp sl d (Query name as)
    = showsUnaryWith (liftShowsPrec sp sl) ("Query " <> show name) d as
  liftShowsPrec sp sl d (Mutation name as)
    = showsUnaryWith (liftShowsPrec sp sl) ("Mutation " <> show name) d as
  liftShowsPrec sp sl d (Subscription name a)
    = showsUnaryWith sp ("Subscription" <> show name) d a

type Value'RAW = Value Name

data Variable a
  = Variable
    { varPos :: Pos
    , varDefinition :: TypeDefinition
    , varValue :: a
    } deriving (Eq, Show)

type Field'RAW = FieldF Value'RAW

type SelectionNode'RAW = SelectionNode Field'RAW

type Fragment'RAW = FragmentF SelectionNode'RAW

data Operation'RAW
  = Operation'RAW
    { _opPos :: Pos
    , _opType :: OperationType
    , _opName :: Maybe Name
    , _opVariables :: HashMap Name (Variable (Maybe JSON.Value))
    , _opSelection :: NonEmpty SelectionNode'RAW
    } deriving (Eq, Show)

type RootNodes'RAW = (HashMap Name Fragment'RAW, NonEmpty Operation'RAW)

type Field = FieldF (Value (Name, Variable JSON.Value))

type Fragment = FragmentF SelectionSet

type FieldSet = Cofree (TreeF Field) Pos

type SelectionSet = SelectionNode Field

type Document = DocumentF FieldSet
