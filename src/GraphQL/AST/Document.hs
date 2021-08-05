{-# LANGUAGE
    DuplicateRecordFields
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TemplateHaskell
#-}

module GraphQL.AST.Document
  ( Typename
  , Name
  , Input
  , Location(..)
  , OperationType(..)
  , ValueF(..)
  , TypeDefinition(..)
  , SelectionNodeF(..)
  , Value'RAW
  , Variable'RAW
  , Field'RAW
  , SelectionNode'RAW
  , Fragment'RAW
  , Operation'RAW
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
import Text.Megaparsec.Pos (SourcePos)

type Typename = Text

type Name = Text

type Input = JSON.Object

data Location = Span SourcePos SourcePos
  deriving (Eq, Show, Ord)

data OperationType
  = Query
  | Mutation
  | Subscription
  deriving (Eq, Show)

data ValueF a r
  = NullVal
  | VarVal a
  | StrVal Text
  | IntVal Int
  | DoubleVal Double
  | BoolVal Bool
  | EnumVal Text
  | ListVal [r]
  | ObjectVal (HashMap Name r)
  deriving (Functor, Foldable, Traversable)

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

type Value'RAW = Cofree (ValueF Name) Location

type Variable'RAW
  = ( TypeDefinition
    , Maybe Value'RAW
    , Location
    )

type Field'RAW
  = ( Maybe Typename         -- typename constraint
    , Maybe Name             -- alias
    , Name                   -- field name
    , HashMap Name Value'RAW -- input values
    )

type SelectionNode'RAW = Cofree SelectionNodeF Location

type Fragment'RAW
  = ( Typename
    , [SelectionNode'RAW]
    , Location
    )

type Operation'RAW
  = ( OperationType
    , Maybe Name
    , HashMap Name Variable'RAW
    , [SelectionNode'RAW]
    , Location
    )

type RootNodes'RAW = (Operation'RAW, HashMap Name Fragment'RAW)

-- | Validated AST nodes

data Field
  = Field
    { typename :: Maybe Typename
    , alias :: Maybe Name
    , name :: Name
    , input :: HashMap Name Value
    }

data Document
  = Document
    { operation :: OperationType
    , name :: Maybe Name
    , selection :: [Selection]
    }

type Value = Cofree (ValueF JSON.Value) (Location, Maybe TypeDefinition)

type Selection = Cofree (TreeF Field) Location
