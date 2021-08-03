{-# LANGUAGE
    OverloadedStrings
#-}

module Test.GraphQL.AST.ParserSpec where

import Test.Hspec

import GHC.Generics (Generic)

import GraphQL.AST.Parser
  ( OperationType(..)
  , VariableDefinition(..)
  , VariableAssignment(..)
  , Field(..)
  , Selection(..)
  , Fragment(..)
  , Document(..)
  , document
  )

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Text (Text)
import Text.Megaparsec (ParseErrorBundle, runParserT)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import Data.Functor.Identity (Identity(..))

parse :: String -> IO (Either (ParseErrorBundle Text Text) Document)
parse f = pure . runIdentity . runParserT document f =<< Text.readFile f

spec :: Spec
spec = describe "GraphQL.AST.Parser" $ do
  it "test/queries/good_input.graphql" $ do
    let
      vars =
        [ ("someVar", TypeVar "SomeType")
        , ("nonNullVar", NonNullVar $ TypeVar "SomeType")
        , ("listVar", NonNullVar $ ListVar $ NonNullVar $ TypeVar "Int")
        ]
      vars'c =
        [ ("nullVal", NullVal)
        , ("boolVal", BoolVal True)
        , ("intVal", IntVal 123)
        , ("doubleVal", DoubleVal 1.23E-6)
        , ("enumVal", EnumVal "MY_ENUM")
        , ("listVal", ListVal [EnumVal "A", EnumVal "B", EnumVal "C"])
        , ("listVal2", ListVal [IntVal 1, IntVal 2, IntVal 3])
        , ("objectVal", ObjectVal [("someVal", Var "someVar"), ("nonNullVal", Var "nonNullVar"), ("listVal", Var "listVar")])
        , ("inlineStr", StrVal "inline string")
        , ("multilineStr", StrVal "    multiline\n\n    \"string\"\n    ")
        ]
      fields =
        [ Node (Field Nothing Nothing "a" [])  []
        , Node (Field Nothing Nothing "b" [])  []
        , Node (Field Nothing Nothing "c" vars'c)  []
        ]
      doc = Document Query Nothing vars fields []
    parse "test/queries/good_input.graphql" `shouldReturn` Right doc


