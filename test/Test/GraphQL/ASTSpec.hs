{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
#-}

module Test.GraphQL.ASTSpec where

import Test.Hspec

import GHC.Generics (Generic)

import GraphQL.Response
import GraphQL.AST
  ( Pos(..)
  , OperationType(..)
  , TypeDefinition(..)
  , ValueF(..)
  , FieldF(..)
  , DocumentF(..)
  , FieldSet
  , parseDocument
  , collectFields
  )

import Control.Monad ((<=<))
import Control.Comonad.Cofree (Cofree(..))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import Text.Megaparsec (runParserT)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import Data.Functor.Base (TreeF(..))
import Data.Functor.Identity (Identity(..))

pos :: Int -> Int -> Pos
pos line col = (Pos line col)

parseTest' :: Maybe Text -> JSON.Value -> String -> IO (V (DocumentF FieldSet))
parseTest' op (JSON.Object i) f = (uncurry collectFields <=< parseDocument f op i) <$> Text.readFile f
parseTest = parseTest' Nothing
parseTestNamed = parseTest' . Just

spec :: Spec
spec = describe "document" $ do
  it "test/queries/good_input.graphql" $ do
    let
      someVarType = NamedType "SomeType"
      nonNullVarType = NonNullType $ NamedType "SomeType"
      listVarType = NonNullType $ ListType $ NonNullType $ NamedType "Int"
      good_input = object
        [ "nonNullVar" .= (420 :: Int)
        , "listVar" .= ([6, 9] :: [Int])
        ]
      bad_input = object
        [ "nonNullVar" .= (Nothing :: Maybe Int)
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
            [ ("someVal", (pos 16 35, Just someVarType) :< NullVal)
            , ("nonNullVal", (pos 17 35, Just nonNullVarType) :< Var (JSON.toJSON (420 :: Int)))
            , ("listVal", (pos 18 35, Just listVarType) :< Var (JSON.toJSON ([6, 9] :: [Int])))
            ] )
          )
        , ("inlineStr", (pos 20 20, Nothing) :< StrVal "inline string")
        , ("multilineStr", (pos 21 20, Nothing) :< StrVal "    multiline\n\n    \"string\"\n    ")
        ]
      doc = Query Nothing $ NE.fromList
        [ (pos 6 3) :< NodeF (Field Nothing Nothing "a" mempty)  []
        , (pos 7 3) :< NodeF (Field Nothing Nothing "b" mempty)  []
        , (pos 8 3) :< NodeF (Field Nothing Nothing "c" vars'c)  []
        ]
    parseTest good_input "test/queries/good_input.graphql" `shouldReturn`
      Right doc
    parseTest bad_input "test/queries/good_input.graphql" `shouldReturn`
      validationError [pos 3 16] "Required variable $nonNullVar is missing from input"
  it "test/queries/bad_input_undefined_variable.graphql" $ do
    parseTest (object []) "test/queries/bad_input_undefined_variable.graphql" `shouldReturn`
      validationError [pos 2 12] "Variable $someVar is not defined"
  it "test/queries/good_selection.graphql" $ do
    let
      vars'b = Map.fromList
        [ ("var", (pos 3 10, Nothing) :< IntVal 420)
        ]
      doc = Query Nothing $ NE.fromList
        [ (pos 2 3) :< NodeF (Field Nothing Nothing "a" mempty)  []
        , (pos 3 3) :< NodeF (Field Nothing Nothing "b" vars'b)  []
        , (pos 4 3) :< NodeF (Field Nothing (Just "alias") "c" mempty)
          [ (pos 4 14) :< NodeF (Field Nothing Nothing "c0" mempty) []
          ]
        , (pos 5 3) :< NodeF (Field Nothing Nothing "ab" mempty)
          [ (pos 6 15) :< NodeF (Field (Just "A") Nothing "a0" mempty) []
          , (pos 12 3) :< NodeF (Field (Just "B") Nothing "b0" mempty) []
          , (pos 17 3) :< NodeF (Field (Just "B") Nothing "b1" mempty)
            [ (pos 17 8) :< NodeF (Field Nothing Nothing "x" mempty) []
            ]
          ]
        ]
    parseTest (object []) "test/queries/good_selection.graphql"
      `shouldReturn` Right doc
  it "test/queries/good_selection_shorthand_query.graphql" $ do
    let
      doc = Query Nothing $ NE.fromList
        [ (pos 1 3) :< NodeF (Field Nothing Nothing "a" mempty)  []
        ]
    parseTest (object []) "test/queries/good_selection_shorthand_query.graphql"
      `shouldReturn` Right doc
  it "test/queries/good_selection_multiple_named_operations.graphql" $ do
    let
      query = Query (Just "Query") $ NE.fromList
        [ (pos 1 15) :< NodeF (Field Nothing Nothing "a" mempty)  []
        ]
      mutation = Mutation (Just "Mutation") $ NE.fromList
        [ (pos 2 21) :< NodeF (Field Nothing Nothing "b" mempty)  []
        ]
      subscription = Subscription (Just "Subscription") $
        (pos 3 29) :< NodeF (Field Nothing Nothing "c" mempty)  []
    parseTestNamed "Query" (object []) "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` Right query
    parseTestNamed "Mutation" (object []) "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` Right mutation
    parseTestNamed "Subscription" (object []) "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` Right subscription
    parseTestNamed "X" (object []) "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` validationError [] "Operation X is not defined"
  it "test/queries/bad_selection_unused_fragment.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_unused_fragment.graphql"
      `shouldReturn` validationError [pos 3 10, pos 4 10] "Document has unused fragments: A, B"
  it "test/queries/bad_selection_missing_operation.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_missing_operation.graphql"
      `shouldReturn` parseError [] "Expected at least one root operation, found none"
  it "test/queries/bad_selection_fragment_cycle.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_fragment_cycle.graphql"
      `shouldReturn` validationError [pos 12 11] "Cycle in fragment A0"
  it "test/queries/bad_selection_duplicated_fragment_names.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_duplicated_fragment_names.graphql"
      `shouldReturn` parseError [pos 5 10] "Duplicated fragment names: A"
  it "test/queries/bad_selection_multiple_unnamed_operations.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_multiple_unnamed_operations.graphql"
      `shouldReturn` validationError [pos 3 1] "Unnamed operations in document with multiple operations"
  it "test/queries/bad_selection_duplicated_operation_names.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_duplicated_operation_names.graphql"
      `shouldReturn` validationError [pos 1 1] "Duplicated operation names: A"
