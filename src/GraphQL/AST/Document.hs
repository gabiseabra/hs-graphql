{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
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
  , Value'RAW
  , Variable'RAW(..)
  , Field'RAW(..)
  , SelectionNode'RAW
  , Fragment'RAW(..)
  , Operation'RAW(..)
  , RootNodes'RAW
  , Field(..)
  , Document(..)
  , Value
  , Selection
  ) where

import Control.Comonad.Cofree (Cofree)
import qualified Data.Aeson as JSON
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

data Pos = Pos { line :: Int, column :: Int } deriving (Eq, Show, Ord)

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

data SelectionNodeF r
  = Node Field'RAW [r]
  | FragmentSpread Name
  | InlineFragment Typename [r]
  deriving (Functor, Foldable, Traversable)

instance Eq1 SelectionNodeF where
  liftEq f (Node a l)           (Node b r)           = a == b && liftEq f l r
  liftEq f (FragmentSpread a)   (FragmentSpread b)   = a == b
  liftEq f (InlineFragment a l) (InlineFragment b r) = a == b && liftEq f l r
  liftEq _ _ _                           = False

instance Show1 SelectionNodeF where
  liftShowsPrec sp sl d (Node a r)           = showsUnaryWith (liftShowsPrec sp sl) ("Node " <> show a) d r
  liftShowsPrec sp sl d (FragmentSpread a)   = (<>) $ "FragmentSpread " <> show a
  liftShowsPrec sp sl d (InlineFragment a r) = showsUnaryWith (liftShowsPrec sp sl) ("InlineFragment " <> show a) d r

type Value'RAW = Cofree (ValueF Name) Pos

data Variable'RAW
  = Variable'RAW
    { _varPos :: Pos
    , _varTypeDef :: TypeDefinition
    , _varDefValue :: Maybe Value'RAW
    } deriving (Eq, Show)

data Field'RAW
  = Field'RAW
    { _fieldType :: Maybe Typename
    , _fieldAlias :: Maybe Name
    , _fieldName :: Name
    , _fieldInput :: HashMap Name Value'RAW
    } deriving (Eq, Show)

type SelectionNode'RAW = Cofree SelectionNodeF Pos

data Fragment'RAW
  = Fragment'RAW
    { _fragPos ::Pos
    , _fragTypename :: Typename
    , _fragSelection :: [SelectionNode'RAW]
    } deriving (Eq, Show)

data Operation'RAW
  = Operation'RAW
    { _opPos :: Pos
    , _opType :: OperationType
    , _opName :: Maybe Name
    , _opVariables :: HashMap Name Variable'RAW
    , _opSelection :: [SelectionNode'RAW]
    } deriving (Eq, Show)

type RootNodes'RAW = (HashMap Name Fragment'RAW, NonEmpty Operation'RAW)

-- | Validated AST nodes

data Field
  = Field
    { fieldType :: Maybe Typename
    , fieldAlias :: Maybe Name
    , fieldName :: Name
    , fieldInput :: HashMap Name Value
    } deriving (Eq, Show)

data Document
  = Document
    { opType :: OperationType
    , opName :: Maybe Name
    , opSelection :: [Selection]
    } deriving (Eq, Show)

type Value = Cofree (ValueF JSON.Value) (Pos, Maybe TypeDefinition)

type Selection = Cofree (TreeF Field) Pos
