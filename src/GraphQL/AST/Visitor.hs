{-# LANGUAGE
    TypeFamilies
  , GADTs
#-}

module GraphQL.AST.Visitor where

import GraphQL.AST.Document
import GraphQL.Response

import Data.Bifunctor (first, second)
import Data.Functor.Foldable (Base)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Comonad.Cofree (Cofree(..))

data ASTLocation
  = DOCUMENT
  | FRAGMENT  -- todo: parametrize these
  | OPERATION -- OperationType
  | SELECTION -- SelectionType
  | FIELD

type family Node loc a where
  Node DOCUMENT  a     = Document       (Selection a)
  Node FRAGMENT  a     = Fragment       (Selection a)
  Node (OPERATION k) a = Operation  k   (Selection a)
  Node (SELECTION k) a = SelectionF k a (Selection a)

data AST loc a where
  DocumentNode  ::        Node DOCUMENT  a -> AST DOCUMENT  a
  FragmentNode  :: Pos -> Node FRAGMENT  a -> AST FRAGMENT  a
  OperationNode :: Pos -> Node OPERATION a -> AST OPERATION a
  SelectionNode :: Pos -> Node SELECTION a -> AST SELECTION a

getNode :: AST loc f a -> Node loc f a
getNode (DocumentNode  a) = a
getNode (FragmentNode  a) = a
getNode (OperationNode a) = a
getNode (SelectionNode a) = a

type Visitor a = forall loc . AST loc a -> V (AST loc a)


-- walk :: Visitor f a -> AST loc f a -> V (NodeOf loc f a)
-- walk f (DocumentNode (Document {..})) =
--   f =<< Document
--     <$> fmap (walk f . FragmentNode) $ fragments
--     <*> fmap (walk f . OperationNode) $ operations
-- walk f (FragmentNode frag) = f =<< traverse (walk f . SelectionNode) frag
-- walk f (OperationNode op) = f =<< traverse (walk f . SelectionNode . ) op
-- walk f (SelectionNode sel) = f =<< traverse (walk f . SelectionNode) op
