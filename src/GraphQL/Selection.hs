{-# LANGUAGE DuplicateRecordFields #-}

module GraphQL.Selection
  ( Selection(..)
  , SelectionTreeF
  , rootSelection
  ) where

import GraphQL.Class
import GraphQL.IO.Input

import Data.Functor.Base (TreeF(..))

data Selection
  = Sel
    { name :: String
    , alias :: Maybe String
    -- , variables :: Variables
    , input :: Input
    , typeConstraint :: Maybe Typename
    } deriving (Show)

type SelectionTreeF = TreeF Selection

rootSelection = NodeF (Sel { name = "data", alias = Nothing, input = mempty, typeConstraint = Nothing })
