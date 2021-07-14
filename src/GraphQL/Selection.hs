{-# LANGUAGE DuplicateRecordFields #-}

module GraphQL.Selection
  ( Selection(..)
  , SelectionTreeF
  , rootSelection
  , rootSelection_
  ) where

import GraphQL.Typeable
import GraphQL.IO.Input

import Data.Functor.Base (TreeF(..))

data Selection
  = Sel
    { name :: String
    , alias :: Maybe String
    , variables :: Variables
    , typeConstraint :: Maybe Typename
    } deriving (Show)

type SelectionTreeF = TreeF Selection

rootSelection = NodeF (Sel { name = "data", alias = Nothing, variables = [], typeConstraint = Nothing })
rootSelection_ = rootSelection . pure
