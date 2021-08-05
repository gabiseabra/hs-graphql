{-# LANGUAGE
    OverloadedStrings
#-}

module Test.GraphQL.ASTSpec where

import Test.Hspec

import GHC.Generics (Generic)

import GraphQL.Error
import GraphQL.AST
  ( Input
  , Pos(..)
  , OperationType(..)
  , TypeDefinition(..)
  , ValueF(..)
  , Field(..)
  , Document(..)
  , document
  )

import Control.Comonad.Cofree (Cofree(..))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import Text.Megaparsec (runParserT)
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Identity (Identity(..))

pos :: Int -> Int -> Pos
pos line col = (Pos line col)

parseTest :: JSON.Value -> String -> IO (V Document)
parseTest (JSON.Object i) f = document i f <$> Text.readFile f

spec :: Spec
spec = describe "document" $ do
  it "test/queries/good_input.graphql" $ do
    let
      someVarType = NamedType "SomeType"
      nonNullVarType = NonNullType $ NamedType "SomeType"
      listVarType = NonNullType $ ListType $ NonNullType $ NamedType "Int"
      input = object
        [ "someVar" .= (Nothing :: Maybe Int)
        , "nonNullVar" .= (420 :: Int)
        , "listVar" .= ([6, 9] :: [Int])
        ]
      vars'c = Map.fromList
        [ ("nullVal", (pos 9 20, Nothing) :< NullVal)
        , ("boolVal", (pos 10 20, Nothing) :< BoolVal True)
        , ("intVal", (pos 11 20, Nothing) :< IntVal 123)
        , ("doubleVal", (pos 12 20, Nothing) :< DoubleVal 1.23E-6)
        , ("enumVal", (pos 13 20, Nothing) :< EnumVal "MY_ENUM")
        , ("listVal", (pos 14 20, Nothing) :< ListVal
            [ (pos 14 22, Nothing) :< EnumVal "A"
            , (pos 14 25, Nothing) :< EnumVal "B"
            , (pos 14 28, Nothing) :< EnumVal "C"
            ]
          )
        , ("listVal2", (pos 15 20, Nothing) :< ListVal
            [ (pos 15 22, Nothing) :< IntVal 1
            , (pos 15 25, Nothing) :< IntVal 2
            , (pos 15 28, Nothing) :< IntVal 3
            ]
          )
        , ("objectVal", (pos 16 20, Nothing) :< ( ObjectVal $ Map.fromList
            [ ("someVal", (pos 16 35, Just someVarType) :< Var (JSON.toJSON (Nothing :: Maybe Int)))
            , ("nonNullVal", (pos 17 35, Just nonNullVarType) :< Var (JSON.toJSON (420 :: Int)))
            , ("listVal", (pos 18 35, Just listVarType) :< Var (JSON.toJSON ([6, 9] :: [Int])))
            ] )
          )
        , ("inlineStr", (pos 20 20, Nothing) :< StrVal "inline string")
        , ("multilineStr", (pos 21 20, Nothing) :< StrVal "    multiline\n\n    \"string\"\n    ")
        ]
      doc = Document Query Nothing
        [ (pos 6 3) :< NodeF (Field Nothing Nothing "a" mempty)  []
        , (pos 7 3) :< NodeF (Field Nothing Nothing "b" mempty)  []
        , (pos 8 3) :< NodeF (Field Nothing Nothing "c" vars'c)  []
        ]
    parseTest input "test/queries/good_input.graphql" `shouldReturn` Right doc


