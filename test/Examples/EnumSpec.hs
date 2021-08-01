{-# LANGUAGE
    TypeFamilies
  , DeriveGeneric
  , DeriveAnyClass
  , OverloadedStrings
  , TypeApplications
#-}

module Examples.EnumSpec where

import Test.Hspec
import Test.Utils

import GHC.Generics (Generic)

import GraphQL.Class (GraphQLType(..))
import GraphQL.Kinds
import GraphQL.Types
import GraphQL.IO.Input

import Control.Monad ((<=<))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as JSON
import Data.Text (Text)

data Enum0
  = Enum_A
  | Enum_B
  | Enum_C
  deriving (Generic)

instance GraphQLType Enum0 where
  type KindOf Enum0 = ENUM
  typename _ = "Enum0"

data Input0 = Input0 { i0 :: Enum0 } deriving (Generic, GraphQLInput)

data A m = A { a0 :: Input0 -> m Enum0 } deriving (Generic)

instance Applicative m => GraphQLType (A m) where
  type KindOf (A m) = OBJECT m
  typename _ = "A"

a :: A IO
a = A { a0 = \(Input0 i0) -> pure i0 }

spec :: Spec
spec = describe "Examples.EnumSpec" $ do
  it "parses enum input and output" $ do
    let
      i = object [ "i0" .= ("ENUM_A" :: String) ]
      s = [ sel "a0" i &: [] ]
      o = object [ "a0" .= ("ENUM_A" :: String) ]
    exec s a `shouldReturn` o
  it "fails with invalid enum value" $ do
    let
      i = object [ "i0" .= ("ENUM_X" :: String) ]
      s = [ sel "a0" i &: [] ]
    eval @(A IO) s `shouldBe` Left "Invalid enum value"
