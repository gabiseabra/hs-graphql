{-# LANGUAGE
    TypeFamilies
  , DeriveGeneric
  , OverloadedStrings
  , TypeApplications
#-}

module Examples.ObjectSpec where

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

data A m
  = A
    { a0 :: () -> m Int
    , a1 :: () -> m (Maybe String)
    , a2 :: () -> m [A m]
    } deriving (Generic)

instance Applicative m => GraphQLType (A m) where
  type KindOf (A m) = OBJECT m
  typename _ = "A"

a :: A IO
a = A { a0 = \_ -> pure 420
      , a1 = \_ -> pure (Just "lmao")
      , a2 = \_ -> pure [a, a, a]
      }

spec :: Spec
spec = describe "Examples.ObjectSpec" $ do
  it "resolves selected fields" $ do
    let
      s = [ sel_ "__typename" &: []
          , sel_ "a0" &: []
          , sel_ "a2" &: [ sel_ "a1" `as` "eyy" &: [] ]
          ]
      o = object
          [ "__typename" .= ("A" :: String)
          , "a0" .= (420 :: Int)
          , "a2" .=
              [ object [ "eyy" .= ("lmao" :: String) ]
              , object [ "eyy" .= ("lmao" :: String) ]
              , object [ "eyy" .= ("lmao" :: String) ]
              ]
          ]
    exec s a `shouldReturn` o
  it "fails with empty selection on objects" $ do
    eval @(A IO) [] `shouldBe` Left "Invalid selection"
  it "fails with non-empty selection on scalars" $ do
    let s = [ sel_ "a0" &: [ sel_ "??" &: [] ] ]
    eval @(A IO) s `shouldBe` Left "Invalid selection"
  it "fails with mismatched typename" $ do
    let s = [ sel_ "a0" `on` "X" &: [] ]
    eval @(A IO) s `shouldBe` Left "Typename mismatch: Expected A, got X"
  it "fails invalid selection" $ do
    let s = [ sel_ "x" &: [] ]
    eval @(A IO) s `shouldBe` Left "Field x doesn't exist on type A"
  it "fails with non-empty selection on __typename" $ do
    let s = [ sel_ "__typename" &: [ sel_ "??" &: [] ] ]
    eval @(A IO) s `shouldBe` Left "Invalid selection: __typename does not have fields"
  it "fails with non-empty input on __typename" $ do
    let
      i = object [ "a" .= True ]
      s = [ sel "__typename" i &: [] ]
    eval @(A IO) s `shouldBe` Left "Invalid selection: __typename does not have arguments"
