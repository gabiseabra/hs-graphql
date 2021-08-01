{-# LANGUAGE
    TypeFamilies
  , DeriveGeneric
  , OverloadedStrings
  , TypeApplications
#-}

module Examples.UnionSpec where

import Test.Hspec
import Test.Utils

import GHC.Generics (Generic)

import GraphQL.Class (GraphQLType(..))
import GraphQL.Kinds
import GraphQL.Types

import Control.Monad ((<=<))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as JSON
import Data.Text (Text)

data A m = A { a0 :: () -> m Int } deriving (Generic)

instance Applicative m => GraphQLType (A m) where
  type KindOf (A m) = OBJECT m
  typename _ = "A"

data B m = B { b0 :: () -> m Int } deriving (Generic)

instance Applicative m => GraphQLType (B m) where
  type KindOf (B m) = OBJECT m
  typename _ = "B"

data AB m
  = AB_A (A m)
  | AB_B (B m)
  deriving (Generic)

instance Applicative m => GraphQLType (AB m) where
  type KindOf (AB m) = UNION m
  typename _ = "AB"

ab :: AB IO
ab = AB_A $ A { a0 = \_ -> pure 420 }

spec :: Spec
spec = describe "Examples.UnionSpec" $ do
  it "resolves union types" $ do
    let
      s = [ sel_ "__typename" &: []
          , sel_ "a0" `on` "A" &: []
          , sel_ "b0" `on` "B" &: []
          ]
      o = object
          [ "__typename" .= ("A" :: String)
          , "a0" .= (420 :: Int)
          ]
    exec s ab `shouldReturn` o
  it "fails with empty selection" $ do
    eval @(AB IO) [] `shouldBe` Left "Invalid selection"
  it "fails with invalid typename" $ do
    let s = [ sel_ "x" `on` "X" &: [] ]
    eval @(AB IO) s `shouldBe` Left "Invalid typename X in union selection"
  it "fails with unspecified typename" $ do
    let s = [ sel_ "a0" &: [] ]
    eval @(AB IO) s `shouldBe` Left "No typename provided for union field"
  it "fails with invalid selection on possible type" $ do
    let s = [ sel_ "a1" `on` "A" &: [] ]
    eval @(AB IO) s `shouldBe` Left "Field a1 doesn't exist on type A"
