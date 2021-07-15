{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.GraphQL.ResolutionSpec where

import Test.Hspec

import GHC.Generics (Generic)

import GraphQL.Class (GraphQLType(..))
import GraphQL.Selection
import GraphQL.Resolution
import GraphQL.Kinds
import GraphQL.Types

import Control.Monad ((<=<))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as JSON
import Data.Fix (Fix(..))
import Data.Functor.Base (TreeF(..))

data A m
  = A
    { a0 :: () -> m Int
    , a1 :: () -> m (A m)
    } deriving (Generic)

instance GraphQLType (A m) where
  type KindOf (A m) = GraphQLObject m (Row (A m))
  typename _ = "A"

a :: A IO
a = A { a0 = \_ -> pure 1, a1 = \_ -> pure a }

sel n r = Fix (NodeF s r)
  where
    s = Sel { name = n, alias = Nothing, variables = [], typeConstraint = Nothing }

testResolver r = pure . foldResolution <=< unfoldResolution @(Fix (ResolutionF JSON.Value)) r

spec :: Spec
spec = describe "GraphQL.Resolution" $ do
  it "works" $ do
    let
      s = [sel "a0" [], sel "a1" [sel "a0" []]]
      o = object
            [ "a0" .= (1 :: Int)
            , "a1" .= object [ "a0" .= (1 :: Int) ]
            ]
    testResolver a s `shouldReturn` o
