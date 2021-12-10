{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  , TypeApplications
  , FlexibleContexts
  , AllowAmbiguousTypes
  , OverloadedStrings
#-}

module Test.Utils where

import GraphQL.TypeSystem (GraphQLOutputType)
import GraphQL.Response (V, Pos(..))
import GraphQL.IO.Output (resolveType)

import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Base (TreeF(..))
import Data.Text (Text)
import qualified Data.Text as Text
import GraphQL.AST.Document (Field(..), ExecutableSelection)

(&:) :: Field JSON.Value -> [ExecutableSelection] -> ExecutableSelection
(&:) s r = Pos 0 0 :< NodeF s r

sel :: Text -> JSON.Value -> Field JSON.Value
sel n (JSON.Object i) = Field Nothing Nothing n i

sel_ :: Text -> Field JSON.Value
sel_ n = Field Nothing Nothing n mempty

as :: Field JSON.Value -> Text -> Field JSON.Value
as s a = s { fieldAlias = Just a }

on :: Field JSON.Value -> Text -> Field JSON.Value
on s a = s { fieldType = Just a }

-- | Validates and executes selection
exec ::
  ( MonadFail m
  , GraphQLOutputType m a
  ) => [ExecutableSelection] -> a -> m JSON.Value
exec = go . resolveType
  where
    go (Left e) _ = fail $ "GraphQLError: " <> show e
    go (Right f) a = f a

-- | Just validates a selection
eval :: forall a. GraphQLOutputType IO a => [ExecutableSelection] -> V ()
eval = second (const ()) . resolveType @IO @a

