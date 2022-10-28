{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.GraphQL.Schema.EnumSpec where

import           Control.Monad                  ( (<=<) )
import qualified Data.Aeson                    as JSON
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified GraphQL.Response              as E
import           GraphQL.Schema
import           Test.Hspec
import           Test.Utils

data Enum0
  = Enum_A
  | Enum_B
  | Enum_C
  deriving (Generic)

instance GraphQLType Enum0 where
  type KIND Enum0 = ENUM
  typeDef = enumDef "Enum0"

newtype Input0 = Input0 { i0 :: Enum0 } deriving (Generic, GraphQLInput)

newtype A m = A { a0 :: Input0 -> m Enum0 } deriving (Generic)

instance (Applicative m) => GraphQLType (A m) where
  type KIND (A m) = OBJECT @m
  typeDef = resolverDef "A"

a :: A IO
a = A { a0 = \(Input0 i0) -> pure i0 }

spec :: Spec
spec = do
  enumSpec

enumSpec :: Spec
enumSpec = describe "enumDef" $ do
  it "parses enum input and output" $ do
    let i = object ["i0" .= ("ENUM_A" :: String)]
        s = [sel "a0" i &: []]
        o = object ["a0" .= ("ENUM_A" :: String)]
    exec a s `shouldReturn` o
  it "fails with invalid enum value" $ do
    let i = object ["i0" .= ("ENUM_X" :: String)]
        s = [sel "a0" i &: []]
    eval @(A IO) s `shouldBe` E.graphQLError
      E.BAD_INPUT_ERROR
      [E.Pos 0 0]
      "Failed to parse ENUM Enum0: \"ENUM_X\" is not a valid value of Enum0"
