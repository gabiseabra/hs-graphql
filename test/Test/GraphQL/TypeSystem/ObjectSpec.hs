{-# LANGUAGE
    TypeFamilies
  , DataKinds
  , PolyKinds
  , DeriveGeneric
  , OverloadedStrings
  , TypeApplications
  , MultiParamTypeClasses
  , NamedFieldPuns
#-}

module Test.GraphQL.TypeSystem.ObjectSpec where

import           Control.Monad ((<=<))
import qualified Data.Aeson as JSON
import           Data.Aeson ((.=), object)
import           Data.Proxy (Proxy(..))
import qualified Data.Map as Map
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

newtype B = B { b0 :: Int } deriving (Generic)

instance GraphQLType B where
  type KIND B = PURE_OBJECT
  typeDef = PureType $ objectDef "B"

b = B 420 :: B

data C = C { c0 :: Int, c1 :: Int } deriving (Generic)

instance GraphQLType C where
  type KIND C = OBJECT @IO
  typeDef
    = ObjectType "C" Nothing
    $ Map.fromList
    [ ("c0", mempty `resolver` \() -> pure . c0)
    , ("c1", mempty `resolver` \() -> pure . c1)
    , ("c2", mempty `resolver` \() C { c0, c1 } -> pure (c0 + c1))
    ]

c = C 69 96 :: C

spec :: Spec
spec = do
  resolverSpec
  objectSpec
  simpleSpec
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
    exec a s `shouldReturn` o

objectSpec :: Spec
objectSpec = describe "objectDef" $ do
  it "resolves" $ do
    let
      s = [ sel_ "__typename" &: []
          , sel_ "b0" &: []
          ]
      o = object
          [ "__typename" .= ("B" :: String)
          , "b0" .= (420 :: Int)
          ]
    exec b s `shouldReturn` o

simpleSpec :: Spec
simpleSpec = describe "simpleDef" $ do
  it "resolves" $ do
    let
      s = [ sel_ "__typename" &: []
          , sel_ "c0" &: []
          , sel_ "c1" &: []
          , sel_ "c2" &: []
          ]
      o = object
          [ "__typename" .= ("C" :: String)
          , "c0" .= (69 :: Int)
          , "c1" .= (96 :: Int)
          , "c2" .= (165 :: Int)
          ]
    exec c s `shouldReturn` o

validationSpec :: Spec
validationSpec = describe "validation" $ do
  it "fails with empty selection on objects" $ do
    eval @(A IO) [] `shouldBe` E.graphQLError E.VALIDATION_ERROR [E.Pos 0 0] "Object type A must have a selection"
  it "fails with non-empty selection on scalars" $ do
    let s = [ sel_ "a0" &: [ sel_ "??" &: [] ] ]
    eval @(A IO) s `shouldBe` E.graphQLError E.VALIDATION_ERROR [E.Pos 0 0] "Scalar type Int cannot have a selection"
  it "fails with mismatched typename" $ do
    let s = [ sel_ "a0" `on` "X" &: [] ]
    eval @(A IO) s `shouldBe` E.graphQLError E.VALIDATION_ERROR [E.Pos 0 0] "Typename mismatch: Expected A, got X"
  it "fails invalid selection" $ do
    let s = [ sel_ "x" &: [] ]
    eval @(A IO) s `shouldBe` E.graphQLError E.VALIDATION_ERROR [E.Pos 0 0] "A does not have a field named \"x\""
  it "fails with non-empty selection on __typename" $ do
    let s = [ sel_ "__typename" &: [ sel_ "??" &: [] ] ]
    eval @(A IO) s `shouldBe` E.graphQLError E.VALIDATION_ERROR [E.Pos 0 0] "Scalar type String cannot have a selection"
  it "fails with non-empty input on __typename" $ do
    let
      i = object [ "a" .= True ]
      s = [ sel "__typename" i &: [] ]
    eval @(A IO) s `shouldBe` E.graphQLError E.VALIDATION_ERROR [E.Pos 0 0] "Field __typename does not have arguments"
