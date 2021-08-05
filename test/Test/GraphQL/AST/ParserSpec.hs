{-# LANGUAGE
    OverloadedStrings
#-}

module Test.GraphQL.AST.ParserSpec where

import Test.Hspec

import GHC.Generics (Generic)

import GraphQL.Error
import GraphQL.AST
  ( Input
  , Pos(..)
  , OperationType(..)
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
spec = describe "GraphQL.AST.Parser" $ do
  it "test/queries/good_input.graphql" $ do
    let
      input = object
        [ "someVar" .= (Nothing :: Maybe Int)
        , "nonNullVar" .= (420 :: Int)
        , "listVar" .= ([6, 9] :: [Int])
        ]
      vars'c = Map.fromList
        [ ("nullVal", (pos 1 1, Nothing) :< NullVal)
        , ("boolVal", (pos 1 1, Nothing) :< BoolVal True)
        , ("intVal", (pos 1 1, Nothing) :< IntVal 123)
        , ("doubleVal", (pos 1 1, Nothing) :< DoubleVal 1.23E-6)
        , ("enumVal", (pos 1 1, Nothing) :< EnumVal "MY_ENUM")
        , ("listVal", (pos 1 1, Nothing) :< ListVal
            [ (pos 1 1, Nothing) :< EnumVal "A"
            , (pos 1 1, Nothing) :< EnumVal "B"
            , (pos 1 1, Nothing) :< EnumVal "C"
            ]
          )
        , ("listVal2", (pos 1 1, Nothing) :< ListVal
            [ (pos 1 1, Nothing) :< IntVal 1
            , (pos 1 1, Nothing) :< IntVal 2
            , (pos 1 1, Nothing) :< IntVal 3
            ]
          )
        , ("objectVal", (pos 1 1, Nothing) :< ( ObjectVal $ Map.fromList
            [ ("someVal", (pos 1 1, Nothing) :< Var (JSON.toJSON (Nothing :: Maybe Int)))
            , ("nonNullVal", (pos 1 1, Nothing) :< Var (JSON.toJSON (420 :: Int)))
            , ("listVal", (pos 1 1, Nothing) :< Var (JSON.toJSON ([6, 9] :: [Int])))
            ] )
          )
        , ("inlineStr", (pos 1 1, Nothing) :< StrVal "inline string")
        , ("multilineStr", (pos 1 1, Nothing) :< StrVal "    multiline\n\n    \"string\"\n    ")
        ]
      doc = Document Query Nothing
        [ (pos 6 3) :< NodeF (Field Nothing Nothing "a" mempty)  []
        , (pos 1 1) :< NodeF (Field Nothing Nothing "b" mempty)  []
        , (pos 1 1) :< NodeF (Field Nothing Nothing "c" vars'c)  []
        ]
    parseTest input "test/queries/good_input.graphql" `shouldReturn` Right doc


