{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.GraphQL.Schema.UnionSpec where

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

newtype A m = A { a0 :: () -> m Int } deriving (Generic)

instance (Applicative m) => GraphQLType (A m) where
  type KIND (A m) = OBJECT @m
  typeDef = resolverDef "A"

newtype B m = B { b0 :: () -> m Int } deriving (Generic)

instance (Applicative m) => GraphQLType (B m) where
  type KIND (B m) = OBJECT @m
  typeDef = resolverDef "B"

data AB m
  = AB_A (A m)
  | AB_B (B m)
  deriving (Generic)

instance (Applicative m) => GraphQLType (AB m) where
  type KIND (AB m) = UNION @m
  typeDef = unionDef "AB"

ab :: AB IO
ab = AB_A $ A { a0 = \_ -> pure 420 }

spec :: Spec
spec = do
  unionSpec
  validationSpec

unionSpec :: Spec
unionSpec = describe "unionDef" $ do
  it "resolves" $ do
    let s =
          [ sel_ "__typename" &: []
          , sel_ "a0" `on` "A" &: []
          , sel_ "b0" `on` "B" &: []
          ]
        o = object ["__typename" .= ("A" :: String), "a0" .= (420 :: Int)]
    exec ab s `shouldReturn` o

validationSpec :: Spec
validationSpec = describe "validation" $ do
  it "fails with empty selection" $ do
    eval @(AB IO) [] `shouldBe` E.graphQLError
      E.VALIDATION_ERROR
      [E.Pos 0 0]
      "Union type AB must have a selection"
  it "fails with invalid typename" $ do
    let s = [sel_ "x" `on` "X" &: []]
    eval @(AB IO) s `shouldBe` E.graphQLError
      E.VALIDATION_ERROR
      [E.Pos 0 0]
      "\"X\" is not a possible type of AB"
  it "fails with unspecified typename" $ do
    let s = [sel_ "a0" &: []]
    eval @(AB IO) s `shouldBe` E.graphQLError
      E.VALIDATION_ERROR
      [E.Pos 0 0]
      "Selections of union types must have a typename"
  it "fails with invalid selection on possible type" $ do
    let s = [sel_ "a1" `on` "A" &: []]
    eval @(AB IO) s `shouldBe` E.graphQLError
      E.VALIDATION_ERROR
      [E.Pos 0 0]
      "A does not have a field named \"a1\""
