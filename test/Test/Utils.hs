{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  , TypeApplications
  , FlexibleContexts
  , AllowAmbiguousTypes
  , OverloadedStrings
#-}

module Test.Utils where

import GraphQL.TypeSystem
import GraphQL.Selection
import GraphQL.Internal
import GraphQL.IO.Input
import GraphQL.IO.Output

import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
import Data.Fix (Fix(..))
import Data.Functor.Base (TreeF(..))
import Data.Text (Text)
import qualified Data.Text as Text

(&:) :: Selection -> [STree] -> STree
(&:) s r = Fix (NodeF s r)

sel :: Text -> JSON.Value -> Selection
sel n (JSON.Object i)
  = Sel
    { name = n
    , input = i
    , alias = Nothing
    , typeConstraint = Nothing
    }

sel_ :: Text -> Selection
sel_ n = sel n (JSON.Object mempty)

as :: Selection -> Text -> Selection
as s a = s { alias = Just a }

on :: Selection -> Text -> Selection
on s a = s { typeConstraint = Just a }

-- | Validates and executes selection
exec ::
  ( MonadFail m
  , GraphQLOutputType m a
  ) => [STree] -> a -> m JSON.Value
exec = go . resolve
  where
    go (Left e) _ = fail $ Text.unpack e
    go (Right f) a = f a

-- | Just validates a selection
eval :: forall a. GraphQLOutputType IO a => [STree] -> V ()
eval = second (const ()) . resolve @a @IO

