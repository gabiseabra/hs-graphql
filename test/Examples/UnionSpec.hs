{-# LANGUAGE
    TypeFamilies
  , DataKinds
  , PolyKinds
  , DeriveGeneric
  , OverloadedStrings
  , TypeApplications
#-}

module Examples.UnionSpec where

import Test.Hspec
import Test.Utils

import GHC.Generics (Generic)

import GraphQL.TypeSystem
import GraphQL.Types

import Control.Monad ((<=<))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as JSON
import Data.Text (Text)

data A m = A { a0 :: () -> m Int } deriving (Generic)

instance (Applicative m) => GraphQLType (A m) where
  type KIND (A m) = OBJECT @m
  typeDef = resolverDef "A"

data B m = B { b0 :: () -> m Int } deriving (Generic)

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

validationSpec :: Spec
validationSpec = describe "validation" $ do
  it "fails with empty selection" $ do
    eval @(AB IO) [] `shouldBe` Left "Union type AB must have a selection"
  it "fails with invalid typename" $ do
    let s = [ sel_ "x" `on` "X" &: [] ]
    eval @(AB IO) s `shouldBe` Left "X is not a possible type of union type AB"
  it "fails with unspecified typename" $ do
    let s = [ sel_ "a0" &: [] ]
    eval @(AB IO) s `shouldBe` Left "Invalid selection with unspecified typename on union type AB"
  it "fails with invalid selection on possible type" $ do
    let s = [ sel_ "a1" `on` "A" &: [] ]
    eval @(AB IO) s `shouldBe` Left "Field a1 does not exist in object of type A"
