{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  , TypeApplications
  , FlexibleContexts
  , AllowAmbiguousTypes
  , OverloadedStrings
#-}

module Test.Utils where

import GraphQL.Selection
import GraphQL.Internal
import GraphQL.Resolution
import GraphQL.IO.Input
import GraphQL.IO.Output


import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
import Data.Fix (Fix(..))
import Data.Functor.Base (TreeF(..))
import Data.Text (Text)

type STree = Fix (TreeF Selection)

(&:) :: Selection -> [STree] -> STree
(&:) s r = Fix (NodeF s r)

sel :: Text -> Input -> Selection
sel n i = Sel { name = n, input = i, alias = Nothing }

sel_ :: Text -> Selection
sel_ n = sel n mempty

as :: Selection -> Text -> Selection
as (Sel { name, input }) a = Sel { name = name, input = input, alias = Just a }

-- | Validates and executes selection
exec ::
  ( MonadFail m
  , GraphQLOutputType m a
  ) => [STree] -> a -> m JSON.Value
exec = go . resolve
  where
    go (Left e) _ = fail e
    go (Right f) a = f a

-- | Just validates a selection
eval :: forall a. GraphQLOutputType IO a => [STree] -> V ()
eval = second (const ()) . resolve @a @IO

