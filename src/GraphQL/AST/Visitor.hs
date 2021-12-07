{-# LANGUAGE
    OverloadedStrings
  , TypeOperators
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  , FlexibleContexts
#-}

module GraphQL.AST.Visitor where

import GraphQL.AST.Document
import GraphQL.Response (V)
import qualified GraphQL.Response as E

import GHC.Generics ((:+:))

import Control.Arrow ((>>>), (&&&))
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad (void)
import qualified Control.Comonad.Trans.Cofree as CofreeT
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.Monadic
import Data.Functor.Identity (Identity(..))
import Data.Functor.Foldable
import Lens.Micro (Lens')
import Control.Lens.Plated
import Control.Monad.State.Lazy (StateT(..))
import qualified Control.Monad.State.Lazy as ST
import Data.Bitraversable (bitraverse)
import Control.Lens.Fold
import Control.Lens.Traversal
import Control.Lens.Indexed

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Visitor a = Document a -> V (Document a)

overlappingFieldsCanBeMerged :: Visitor (Selection (Field a))
overlappingFieldsCanBeMerged doc = do
  undefined
  where
    conflicts f0 f1 = fieldAlias f0 == fieldAlias f1 || fieldName f0 == fieldName f1 && (fieldArgs f0 /= fieldArgs f1 || fieldType f0 /= fieldType f1)

validDefaultValues :: Visitor a
validDefaultValues doc = do
  mapMOf_ (_docOperations . traverse . _opVariables . traverse) validation doc
  pure doc
  where
    validation var@(Variable _ _ Nothing) = pure ()
    validation var@(Variable _ def (Just (pos:<val))) =
      if checkTypeDefinition def (pos:<val)
        then pure ()
        else E.validationError [pos]
              $ "Expected "
              <> Text.pack (show def)
              <> ", found "
              <> (LazyText.toStrict . JSON.encodeToLazyText . cata (JSON.toJSON . CofreeT.tailF) $ (pos:<val))

test = () :< Node 1 [() :< InlineFragment "A" ((() :< Node 2 [() :< FragmentSpread "X", ():<Node 4 []]) :| [() :< Node 3 []])]
