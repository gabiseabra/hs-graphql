{-# LANGUAGE DuplicateRecordFields #-}

module GraphQL.Selection
  ( Selection(..)
  ) where

import GraphQL.TypeSystem (Typename)
import GraphQL.IO.Input

import qualified Data.Aeson as JSON
import Data.Text (Text)

data Selection
  = Sel
    { name :: Text
    , alias :: Maybe Text
    -- , variables :: Variables
    , input :: JSON.Object
    , typeConstraint :: Maybe Typename
    } deriving (Show)
