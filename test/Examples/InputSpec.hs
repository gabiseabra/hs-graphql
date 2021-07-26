{-# LANGUAGE
    TypeFamilies
  , DeriveGeneric
  , OverloadedStrings
  , NamedFieldPuns
  , DeriveAnyClass
  , TypeApplications
#-}

module Examples.InputSpec where

import Test.Hspec
import Test.Utils

import GHC.Generics (Generic)

import GraphQL.Class (GraphQLType(..))
import GraphQL.Kinds
import GraphQL.Types
import GraphQL.IO.Input

import Control.Monad ((<=<))
import Data.Aeson ((.=), object)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as JSON
import Data.Text (Text)

data AnInputObject
  = AnInputObject
    { io_0 :: String
    , io_1 :: Maybe String
    } deriving (Generic)

instance Show AnInputObject where show (AnInputObject { io_0, io_1 }) = fromMaybe io_0 io_1

instance GraphQLType AnInputObject where
  type KindOf AnInputObject = GraphQLInputObject
  typename _ = "AnInputObject"

-- | Completely optional input
data Input0
  = Input0
    { i0_0 :: Maybe Int
    } deriving (Generic, GraphQLInput)

-- | Input with one required field
data Input1
  = Input1
    { i1_0 :: Int
    , i1_1 :: Maybe AnInputObject
    } deriving (Generic, GraphQLInput)

data A m
  = A
    { a0 :: ()     -> m Int
    , a1 :: ()     -> m (Maybe String)
    , a2 :: Input0 -> m Int
    , a3 :: Input1 -> m (A m)
    } deriving (Generic)

instance Applicative m => GraphQLType (A m) where
  type KindOf (A m) = GraphQLObject m
  typename _ = "A"

a :: Input1 -> (A IO)
a (Input1 { i1_0, i1_1 })
  = A { a0 = \_ -> pure i1_0
      , a1 = \_ -> pure (fmap show i1_1)
      , a2 = \(Input0 { i0_0 }) -> pure (fromMaybe 0 i0_0)
      , a3 = pure . a
      }

a' = a (Input1 { i1_0 = 420, i1_1 = Nothing })

spec :: Spec
spec = describe "Example.InputSpec" $ do
  it "parses valid input" $ do
    let
      i = object
            [ "i1_0" .= (69 :: Int)
            , "i1_1" .= object [ "io_0" .= ("lmao" :: String) ]
            ]
      s = [ sel_ "a0" &: []
          , sel_ "a1" &: []
          , sel_ "a2" &: []
          , sel "a3" i &:
            [ sel_ "a0" &: []
            , sel_ "a1" `as` "eyy" &: []
            ]
          ]
      o = object
            [ "a0" .= (420 :: Int)
            , "a1" .= (Nothing :: Maybe ())
            , "a2" .= (0 :: Int)
            , "a3" .= object
              [ "a0" .= (69 :: Int)
              , "eyy" .= ("lmao" :: String)
              ]
            ]
    exec s a' `shouldReturn` o
  it "fails on invalid input values" $ do
    let
      i = object [ "i1_0" .= ("lmao" :: String) ]
      s = [ sel "a3" i &: [ sel_ "a0" &: [] ] ]
    eval @(A IO) s `shouldBe` Left "parsing Int failed, expected Number, but encountered String"
  it "fails on missing input fields" $ do
    let
      s = [ sel_ "a3" &: [ sel_ "a0" &: [] ] ]
    eval @(A IO) s `shouldBe` Left "parsing Int failed, expected Number, but encountered Null"
