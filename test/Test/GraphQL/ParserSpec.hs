{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Test.GraphQL.ParserSpec where

import           Test.Hspec

import           GHC.Generics                   ( Generic )

import           GraphQL.Parser
import qualified GraphQL.Response              as E

import           Control.Comonad.Cofree         ( Cofree(..) )
import           Control.Monad                  ( (<=<) )
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Bifunctor                 ( first )
import           Data.Fix                       ( Fix(..) )
import           Data.Functor.Base              ( TreeF(..) )
import           Data.Functor.Identity          ( Identity(..) )
import qualified Data.HashMap.Strict           as HashMap
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
                                                ( readFile )
import           Test.Utils
import           Text.Megaparsec                ( runParserT )
import           Text.Megaparsec.Error          ( ParseErrorBundle )

parseTest'
  :: Maybe Text
  -> JSON.Value
  -> String
  -> IO (Either (NonEmpty E.GraphQLError) ExecutableOperation)
parseTest' opName (JSON.Object input) fileName =
  (getExecutableOperation input opName <=< basicRules <=< parseDocument fileName
    )
    <$> Text.readFile fileName
parseTest = parseTest' Nothing
parseTestNamed = parseTest' . Just

spec :: Spec
spec = describe "document" $ do
  it "test/queries/good_input.graphql" $ do
    let
      someVarType    = Fix . NamedType $ "SomeType"
      nonNullVarType = Fix . NonNullType $ someVarType
      listVarType =
        Fix
          . NonNullType
          . Fix
          . ListType
          . Fix
          . NonNullType
          . Fix
          . NamedType
          $ "Int"
      vars = HashMap.fromList
        [ ("someVar"   , Variable (E.Pos 2 13) someVarType Nothing)
        , ("nonNullVar", Variable (E.Pos 3 16) nonNullVarType Nothing)
        , ("listVar"   , Variable (E.Pos 4 13) listVarType Nothing)
        ]
      good_input =
        object ["nonNullVar" .= (420 :: Int), "listVar" .= ([6, 9] :: [Int])]
      bad_input =
        object
          [ "nonNullVar" .= (Nothing :: Maybe Int)
          , "listVar" .= ([6, 9] :: [Int])
          ]
      val'c = HashMap.fromList
        [ ("nullVal"  , JSON.Null)
        , ("boolVal"  , JSON.toJSON True)
        , ("intVal"   , JSON.toJSON (123 :: Int))
        , ("doubleVal", JSON.toJSON (1.23E-6 :: Float))
        , ("enumVal"  , JSON.String "MY_ENUM")
        , ("listVal", JSON.toJSON (["A", "B", "C"] :: [String]))
        , ("listVal2" , JSON.toJSON ([1, 2, 3] :: [Int]))
        , ( "objectVal"
          , object
            [ "someVal" .= JSON.Null
            , "nonNullVal" .= JSON.toJSON (420 :: Int)
            , "listVal" .= JSON.toJSON ([6, 9] :: [Int])
            ]
          )
        , ("inlineStr"   , JSON.String "inline string")
        , ("multilineStr", JSON.String "    multiline\n\n    \"string\"\n    ")
        ]
      op = Query (E.Pos 1 1) Nothing vars $ NE.fromList
        [ E.Pos 6 3 :< NodeF (Field Nothing Nothing "a" mempty) []
        , E.Pos 7 3 :< NodeF (Field Nothing Nothing "b" mempty) []
        , E.Pos 8 3 :< NodeF (Field Nothing Nothing "c" val'c) []
        ]
    parseTest good_input "test/queries/good_input.graphql"
      `shouldReturn` Right op
    parseTest bad_input "test/queries/good_input.graphql"
      `shouldReturn` graphQLError
                       E.VALIDATION_ERROR
                       (Just [E.Pos 3 16])
                       Nothing
                       "Required variable $nonNullVar is missing from input"
  it "test/queries/bad_input_undefined_variable.graphql" $ do
    parseTest (object []) "test/queries/bad_input_undefined_variable.graphql"
      `shouldReturn` graphQLError E.VALIDATION_ERROR
                                  (Just [E.Pos 2 12])
                                  Nothing
                                  "Variable $someVar is not defined"
  it "test/queries/good_selection.graphql" $ do
    let val'b = HashMap.fromList [("var", JSON.toJSON (420 :: Int))]
        op    = Query (E.Pos 1 1) Nothing mempty $ NE.fromList
          [ E.Pos 2 3 :< NodeF (Field Nothing Nothing "a" mempty) []
          , E.Pos 3 3 :< NodeF (Field Nothing Nothing "b" val'b) []
          , E.Pos 4 3 :< NodeF
            (Field Nothing (Just "alias") "c" mempty)
            [E.Pos 4 14 :< NodeF (Field Nothing Nothing "c0" mempty) []]
          , E.Pos 5 3 :< NodeF
            (Field Nothing Nothing "ab" mempty)
            [ E.Pos 6 15 :< NodeF (Field (Just "A") Nothing "a0" mempty) []
            , E.Pos 12 3 :< NodeF (Field (Just "B") Nothing "b0" mempty) []
            , E.Pos 17 3 :< NodeF
              (Field (Just "B") Nothing "b1" mempty)
              [E.Pos 17 8 :< NodeF (Field Nothing Nothing "x" mempty) []]
            ]
          ]
    parseTest (object []) "test/queries/good_selection.graphql"
      `shouldReturn` Right op
  it "test/queries/good_selection_shorthand_query.graphql" $ do
    let op = Query (E.Pos 1 1) Nothing mempty $ NE.fromList
          [E.Pos 1 3 :< NodeF (Field Nothing Nothing "a" mempty) []]
    parseTest (object []) "test/queries/good_selection_shorthand_query.graphql"
      `shouldReturn` Right op
  it "test/queries/good_selection_multiple_named_operations.graphql" $ do
    let query = Query (E.Pos 1 1) (Just "Query") mempty $ NE.fromList
          [E.Pos 1 15 :< NodeF (Field Nothing Nothing "a" mempty) []]
        mutation = Mutation (E.Pos 2 1) (Just "Mutation") mempty $ NE.fromList
          [E.Pos 2 21 :< NodeF (Field Nothing Nothing "b" mempty) []]
        subscription =
          Subscription (E.Pos 3 1) (Just "Subscription") mempty
            $  E.Pos 3 29
            :< NodeF (Field Nothing Nothing "c" mempty) []
    parseTestNamed
        "Query"
        (object [])
        "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` Right query
    parseTestNamed
        "Mutation"
        (object [])
        "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` Right mutation
    parseTestNamed
        "Subscription"
        (object [])
        "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` Right subscription
    parseTestNamed
        "X"
        (object [])
        "test/queries/good_selection_multiple_named_operations.graphql"
      `shouldReturn` graphQLError E.VALIDATION_ERROR
                                  Nothing
                                  Nothing
                                  "Operation X is not defined"
  it "test/queries/bad_selection_unused_fragment.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_unused_fragment.graphql"
      `shouldReturn` graphQLError E.VALIDATION_ERROR
                                  (Just [E.Pos 4 10, E.Pos 3 10])
                                  Nothing
                                  "Document has unused fragments: B, A"
  it "test/queries/bad_selection_missing_operation.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_missing_operation.graphql"
      `shouldReturn` graphQLError
                       E.SYNTAX_ERROR
                       Nothing
                       Nothing
                       "Expected at least one root operation, found none"
  it "test/queries/bad_selection_fragment_cycle.graphql" $ do
    parseTest (object []) "test/queries/bad_selection_fragment_cycle.graphql"
      `shouldReturn` graphQLError E.VALIDATION_ERROR
                                  (Just [E.Pos 12 11])
                                  Nothing
                                  "Cycle in fragment A0"
  it "test/queries/bad_selection_duplicated_fragment_names.graphql" $ do
    parseTest (object [])
              "test/queries/bad_selection_duplicated_fragment_names.graphql"
      `shouldReturn` graphQLError E.VALIDATION_ERROR
                                  (Just [E.Pos 5 10, E.Pos 6 10])
                                  Nothing
                                  "Duplicated fragment name A"
  it "test/queries/bad_selection_multiple_unnamed_operations.graphql" $ do
    parseTest (object [])
              "test/queries/bad_selection_multiple_unnamed_operations.graphql"
      `shouldReturn` graphQLError
                       E.VALIDATION_ERROR
                       (Just [E.Pos 3 1])
                       Nothing
                       "Unnamed operation in document with multiple operations"
  it "test/queries/bad_selection_duplicated_operation_names.graphql" $ do
    parseTest (object [])
              "test/queries/bad_selection_duplicated_operation_names.graphql"
      `shouldReturn` graphQLError E.VALIDATION_ERROR
                                  (Just [E.Pos 1 1, E.Pos 2 1])
                                  Nothing
                                  "Duplicated operation name A"
