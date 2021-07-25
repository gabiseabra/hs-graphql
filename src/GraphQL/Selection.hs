{-# LANGUAGE DuplicateRecordFields #-}

module GraphQL.Selection
  ( Selection(..)
  ) where

import GraphQL.Class
import GraphQL.IO.Input

import Data.Functor.Base (TreeF(..))
import Data.Text (Text)

data Selection
  = Sel
    { name :: Text
    , alias :: Maybe Text
    -- , variables :: Variables
    , input :: Input
    , typeConstraint :: Maybe Typename
    } deriving (Show)
