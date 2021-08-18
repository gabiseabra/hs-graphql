{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveGeneric
  , DeriveAnyClass
#-}

module GraphQL.AST.Document
  ( Typename
  , Name
  , Input
  , Pos(..)
  , mkPos
  , OperationType(..)
  , ValueF(..)
  , TypeDefinition(..)
  , SelectionNodeF(..)
  , SelectionNode
  , FieldF(..)
  , FragmentF(..)
  , Document(..)
  , Value'RAW
  , Variable'RAW(..)
  , Field'RAW
  , SelectionNode'RAW
  , Fragment'RAW
  , Operation'RAW(..)
  , RootNodes'RAW
  , Value
  , Field
  , Fragment
  , SelectionTree
  , SelectionSet
  ) where

import GHC.Generic (Generic)

import Control.Comonad.Cofree (Cofree)
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Fix (Fix)
import Data.Functor.Base (TreeF)
import Data.HashMap.Strict (HashMap)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Functor.Classes (Show1(..), Eq1(..), showsUnaryWith)
import qualified Text.Megaparsec.Pos as P

type Typename = Text

type Name = Text

type Input = JSON.Object

data Pos = Pos { line :: Int, column :: Int }
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

mkPos :: P.SourcePos -> Pos
mkPos p = Pos (P.unPos $ P.sourceLine p) (P.unPos $ P.sourceColumn p)

data OperationType
  = Query
  | Mutation
  | Subscription
  deriving (Eq, Show)

data ValueF a r
  = NullVal
  | Var a
  | StrVal Text
  | IntVal Int
  | DoubleVal Double
  | BoolVal Bool
  | EnumVal Text
  | ListVal [r]
  | ObjectVal (HashMap Name r)
  deriving (Functor, Foldable, Traversable)

instance Eq a => Eq1 (ValueF a) where
  liftEq _ NullVal        NullVal        = True
  liftEq _ (Var a)        (Var b)        = a == b
  liftEq _ (StrVal a)     (StrVal b)     = a == b
  liftEq _ (IntVal a)     (IntVal b)     = a == b
  liftEq _ (DoubleVal a)  (DoubleVal b)  = a == b
  liftEq _ (BoolVal a)    (BoolVal b)    = a == b
  liftEq _ (EnumVal a)    (EnumVal b)    = a == b
  liftEq f (ListVal as)   (ListVal bs)   = liftEq f as bs
  liftEq f (ObjectVal as) (ObjectVal bs) = liftEq f as bs
  liftEq _ _ _                           = False

instance Show a => Show1 (ValueF a) where
  liftShowsPrec _  _  _ NullVal        = (<>) $ "NullVal"
  liftShowsPrec _  _  _ (Var a)        = (<>) $ "Var " <> show a
  liftShowsPrec _  _  _ (StrVal a)     = (<>) $ "StrVal " <> show a
  liftShowsPrec _  _  _ (IntVal a)     = (<>) $ "IntVal " <> show a
  liftShowsPrec _  _  _ (DoubleVal a)  = (<>) $ "DoubleVal " <> show a
  liftShowsPrec _  _  _ (BoolVal a)    = (<>) $ "BoolVal " <> show a
  liftShowsPrec _  _  _ (EnumVal a)    = (<>) $ "EnumVal " <> show a
  liftShowsPrec sp sl d (ListVal as)   = showsUnaryWith (liftShowsPrec sp sl) "ListVal" d as
  liftShowsPrec sp sl d (ObjectVal as) = showsUnaryWith (liftShowsPrec sp sl) "ObjectVal" d as

instance
  ( JSON.ToJSON a
  , Recursive r
  , Base r ~ ValueF a
  ) => JSON.ToJSON (ValueF a r) where
  toJSON NullVal       = JSON.Null
  toJSON (Var a)       = JSON.toJSON a
  toJSON (StrVal a)    = JSON.toJSON a
  toJSON (IntVal a)    = JSON.toJSON a
  toJSON (DoubleVal a) = JSON.toJSON a
  toJSON (BoolVal a)   = JSON.toJSON a
  toJSON (EnumVal a)   = JSON.toJSON a
  toJSON (ListVal r)   = JSON.toJSON $ fmap (JSON.toJSON . project) r
  toJSON (ObjectVal r) = JSON.Object $ fmap (JSON.toJSON . project) r

data TypeDefinition
  = ListType TypeDefinition
  | NonNullType TypeDefinition
  | NamedType Typename
  deriving (Eq, Show)

data FieldF a
  = Field
    { fieldType :: Maybe Typename
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
  | InlineFragment Typename [r]
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
    , fragTypename :: Typename
    , fragSelection :: [a]
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

data Document a
  = Document
    { opType :: OperationType
    , opName :: Maybe Name
    , opSelection :: [a]
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq1 Document where
  liftEq f (Document t name as) (Document t' name' bs)
    =  t == t'
    && name == name'
    && liftEq f as bs

instance Show1 Document where
  liftShowsPrec sp sl d (Document t name as)
    = showsUnaryWith
      (liftShowsPrec sp sl)
      ("Document " <> show t <> " " <> show name)
      d as

type Value'RAW = Cofree (ValueF Name) Pos

data Variable'RAW
  = Variable'RAW
    { _varPos :: Pos
    , _varTypeDef :: TypeDefinition
    , _varDefValue :: Maybe Value'RAW
    } deriving (Eq, Show)

type Field'RAW = FieldF Value'RAW

type SelectionNode'RAW = SelectionNode Field'RAW

type Fragment'RAW = FragmentF SelectionNode'RAW

data Operation'RAW
  = Operation'RAW
    { _opPos :: Pos
    , _opType :: OperationType
    , _opName :: Maybe Name
    , _opVariables :: HashMap Name Variable'RAW
    , _opSelection :: [SelectionNode'RAW]
    } deriving (Eq, Show)

type RootNodes'RAW = (HashMap Name Fragment'RAW, NonEmpty Operation'RAW)

type Value = Cofree (ValueF JSON.Value) (Pos, Maybe TypeDefinition)

type Field = FieldF Value

type Fragment = FragmentF SelectionSet

type SelectionTree = Cofree (TreeF Field) Pos

type SelectionSet = SelectionNode Field
