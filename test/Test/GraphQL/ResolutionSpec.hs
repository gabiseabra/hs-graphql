{-# LANGUAGE
    TypeFamilies
  , BlockArguments
  , UndecidableInstances
  , DeriveGeneric
  , OverloadedStrings
  , TypeApplications
  , FlexibleContexts
#-}

module Test.GraphQL.ResolutionSpec where

import Test.Hspec

import GHC.Generics (Generic)

import GraphQL.Class (GraphQLType(..))
import GraphQL.Selection
import GraphQL.Resolution
import GraphQL.Kinds
import GraphQL.Types

import Control.Monad ((<=<))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as JSON
import Data.Fix (Fix(..))
import Data.Functor.Base (TreeF(..))
import Data.Text (Text)

data A m
  = A
    { a0 :: () -> m Int
    , a1 :: () -> m Text
    , as :: () -> m [A m]
    } deriving (Generic)

instance Applicative m => GraphQLType (A m) where
  type KindOf (A m) = GraphQLObject m (Row (A m))
  typename _ = "A"

a :: A IO
a = A { a0 = \_ -> pure 420
      , a1 = \_ -> pure "lmao"
      , as = \_ -> pure [a, a, a]
      }

sel n r = Fix (NodeF s r)
  where
    s = Sel { name = n, alias = Nothing, typeConstraint = Nothing }

run :: MonadFail m => Either String (a -> m b) -> a -> m b
run (Left e) _ = fail e
run (Right f) a = f a

spec :: Spec
spec = describe "GraphQL.Resolution" $ do
  it "works" $ do
    let
      s = [sel "a0" [], sel "as" [sel "a1" []]]
      o = object
            [ "a0" .= (420 :: Int)
            , "as" .=
                [ object [ "a1" .= ("lmao" :: String) ]
                , object [ "a1" .= ("lmao" :: String) ]
                , object [ "a1" .= ("lmao" :: String) ]
                ]
            ]
    run (resolve s) a `shouldReturn` o
