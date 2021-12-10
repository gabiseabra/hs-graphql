{-# LANGUAGE
    TypeFamilies
  , DataKinds
  , PolyKinds
  , DeriveGeneric
  , OverloadedStrings
  , TypeApplications
  , MultiParamTypeClasses
#-}

module Examples.ObjectSpec where

import           Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import           Data.Aeson ((.=), object)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified GraphQL.Response as E
import           GraphQL.TypeSystem
import           GraphQL.Types
import           Test.Hspec
import           Test.Utils

data A m
  = A
    { a0 :: () -> m Int
    , a1 :: () -> m (Maybe String)
    , a2 :: () -> m [A m]
    } deriving (Generic)

instance (Applicative m) => GraphQLType (A m) where
  type KIND (A m) = OBJECT @m
  typeDef = resolverDef "A"

a :: A IO
a = A { a0 = \_ -> pure 420
      , a1 = \_ -> pure (Just "lmao")
      , a2 = \_ -> pure [a, a, a]
      }

data B
  = B
    { b0 :: Int
    , b1 :: Maybe String
    }

instance GraphQLField IO B "test_b0" where
  type OutputOf B "test_b0" = Int
  resolver _ = pure . b0

instance GraphQLField IO B "test_b1" where
  type OutputOf B "test_b1" = Maybe Text
  resolver _ = pure . fmap Text.pack . b1

instance GraphQLField IO B "test_b2" where
  type OutputOf B "test_b2" = [B]
  resolver _ = pure . replicate 2

instance GraphQLType B where
  type KIND B = OBJECT @IO
  typeDef = canonicalDef "B" $ Proxy @["test_b0", "test_b1", "test_b2"]

b = B 420 (Just "eyy") :: B

newtype C = C { c0 :: Int } deriving (Generic)

instance GraphQLType C where
  type KIND C = OBJECT @IO
  typeDef = objectDef "C"

c = C 420 :: C

spec :: Spec
spec = do
  resolverSpec
  canonicalSpec
  objectSpec
  validationSpec

resolverSpec :: Spec
resolverSpec = describe "resolverDef" $ do
  it "resolves" $ do
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

canonicalSpec :: Spec
canonicalSpec = describe "canonicalDef" $ do
  it "resolves" $ do
    let
      s = [ sel_ "__typename" &: []
          , sel_ "test_b0" &: []
          , sel_ "test_b2" &: [ sel_ "test_b1" &: [] ]
          ]
      o = object
          [ "__typename" .= ("B" :: String)
          , "test_b0" .= (420 :: Int)
          , "test_b2" .=
              [ object [ "test_b1" .= ("eyy" :: String) ]
              , object [ "test_b1" .= ("eyy" :: String) ]
              ]
          ]
    exec s b `shouldReturn` o

objectSpec :: Spec
objectSpec = describe "objectDef" $ do
  it "resolves" $ do
    let
      s = [ sel_ "__typename" &: []
          , sel_ "c0" &: []
          ]
      o = object
          [ "__typename" .= ("C" :: String)
          , "c0" .= (420 :: Int)
          ]
    exec s c `shouldReturn` o

validationSpec :: Spec
validationSpec = describe "validation" $ do
  it "fails with empty selection on objects" $ do
    eval @(A IO) [] `shouldBe` E.validationError [] "Object type A must have a selection"
  it "fails with non-empty selection on scalars" $ do
    let s = [ sel_ "a0" &: [ sel_ "??" &: [] ] ]
    eval @(A IO) s `shouldBe` E.validationError [] "Scalar type Int cannot have a selection"
  it "fails with mismatched typename" $ do
    let s = [ sel_ "a0" `on` "X" &: [] ]
    eval @(A IO) s `shouldBe` E.validationError [E.Pos 0 0] "Typename mismatch: Expected A, got X"
  it "fails invalid selection" $ do
    let s = [ sel_ "x" &: [] ]
    eval @(A IO) s `shouldBe` E.validationError [E.Pos 0 0] "Field x does not exist in object of type A"
  it "fails with non-empty selection on __typename" $ do
    let s = [ sel_ "__typename" &: [ sel_ "??" &: [] ] ]
    eval @(A IO) s `shouldBe` E.validationError [E.Pos 0 0] "Field __typename cannot have a selection"
  it "fails with non-empty input on __typename" $ do
    let
      i = object [ "a" .= True ]
      s = [ sel "__typename" i &: [] ]
    eval @(A IO) s `shouldBe` E.validationError [E.Pos 0 0] "Field __typename does not have arguments"
