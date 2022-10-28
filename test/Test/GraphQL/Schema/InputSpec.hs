{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.GraphQL.Schema.InputSpec where

import           Control.Monad                  ( (<=<) )
import qualified Data.Aeson                    as JSON
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified GraphQL.Response              as E
import           GraphQL.Schema
import           Test.Hspec
import           Test.Utils

data AnInputObject = AnInputObject
  { io_0 :: String
  , io_1 :: Maybe String
  }
  deriving Generic

instance Show AnInputObject where
  show (AnInputObject { io_0, io_1 }) = fromMaybe io_0 io_1

instance GraphQLType AnInputObject where
  type KIND AnInputObject = INPUT_OBJECT
  typeDef = inputObjectDef "AnInputObject"

-- | Completely optional input
data Input0 = Input0
  { i0_0 :: Maybe Int
  }
  deriving (Generic, GraphQLInput)

-- | Input with one required field
data Input1 = Input1
  { i1_0 :: Int
  , i1_1 :: Maybe AnInputObject
  }
  deriving (Generic, GraphQLInput)

data A m = A
  { a0 :: () -> m Int
  , a1 :: () -> m (Maybe String)
  , a2 :: Input0 -> m Int
  , a3 :: Input1 -> m (A m)
  }
  deriving Generic

instance (Applicative m) => GraphQLType (A m) where
  type KIND (A m) = OBJECT @m
  typeDef = resolverDef "A"

a :: Input1 -> A IO
a (Input1 { i1_0, i1_1 }) = A
  { a0 = \_ -> pure i1_0
  , a1 = \_ -> pure (fmap show i1_1)
  , a2 = \(Input0 { i0_0 }) -> pure (fromMaybe 0 i0_0)
  , a3 = pure . a
  }

a' = a (Input1 { i1_0 = 420, i1_1 = Nothing })

spec :: Spec
spec = do
  inputObjectSpec
  validationSpec

inputObjectSpec :: Spec
inputObjectSpec = describe "inputObjectDef" $ do
  it "parses" $ do
    let i =
          object
            [ "i1_0" .= (69 :: Int)
            , "i1_1" .= object ["io_0" .= ("lmao" :: String)]
            ]
        s =
          [ sel_ "a0" &: []
          , sel_ "a1" &: []
          , sel_ "a2" &: []
          , sel "a3" i &: [sel_ "a0" &: [], sel_ "a1" `as` "eyy" &: []]
          ]
        o = object
          [ "a0" .= (420 :: Int)
          , "a1" .= (Nothing :: Maybe ())
          , "a2" .= (0 :: Int)
          , "a3" .= object ["a0" .= (69 :: Int), "eyy" .= ("lmao" :: String)]
          ]
    exec a' s `shouldReturn` o

validationSpec :: Spec
validationSpec = describe "validation" $ do
  it "fails with invalid input values" $ do
    let i = object ["i1_0" .= ("lmao" :: String)]
        s = [sel "a3" i &: [sel_ "a0" &: []]]
    eval @(A IO) s `shouldBe` graphQLError
      E.BAD_INPUT_ERROR
      (Just [E.Pos 0 0])
      (Just ["a3"])
      "Failed to parse SCALAR Int: parsing Int failed, expected Number, but encountered String"
  it "fails with missing input fields" $ do
    let s = [sel_ "a3" &: [sel_ "a0" &: []]]
    eval @(A IO) s `shouldBe` graphQLError
      E.BAD_INPUT_ERROR
      (Just [E.Pos 0 0])
      (Just ["a3"])
      "Failed to parse SCALAR Int: parsing Int failed, expected Number, but encountered Null"
