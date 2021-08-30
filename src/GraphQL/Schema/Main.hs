import Data.Functor.Foldable (Base)

module GraphQL.Schema.Main where

data ASTLocation
  = DOCUMENT
  | FRAGMENT  -- todo: parametrize these
  | OPERATION -- OperationType
  | SELECTION -- SelectionType
  | FIELD

type family NodeF loc f a where
  NodeF DOCUMENT  f a = Const (Document  (f a))
  NodeF FRAGMENT  f a = Const (Fragment  (f a))
  NodeF OPERATION f a = Const (Operation (f a))
  NodeF SELECTION f a = Base             (f a)
  NodeF FIELD     f a = Base              a

data tail :> head = tail :> head

data AST loc f a where
  DocumentNode  :: NodeOf DOCUMENT  f a x -> AST DOCUMENT  f a
  FragmentNode  :: NodeOf FRAGMENT  f a x -> AST FRAGMENT  f a
  OperationNode :: NodeOf OPERATION f a x -> AST OPERATION f a
  SelectionNode :: NodeOf SELECTION f a x -> AST SELECTION f a
  FieldNode     :: NodeOf FIELD     f a x -> AST FIELD     f a

class Visitor t f a where visit :: NodeF loc f a -> V (NodeF loc f a)
instance Visitor (Document (f a) -> V (Document (f a))) f a where
  visit :: NodeF loc f a -> V (NodeF loc f a)


walk :: Visitor f a -> AST loc f a -> V (NodeOf loc f a)
walk f (DocumentNode (Document {..})) =
  f =<< Document
    <$> fmap (walk f . FragmentNode) $ fragments
    <*> fmap (walk f . OperationNode) $ operations
walk f (FragmentNode frag) = f =<< traverse (walk f . SelectionNode) frag
walk f (OperationNode op) = f =<< traverse (walk f . SelectionNode . ) op
walk f (SelectionNode sel) = f =<< traverse (walk f . SelectionNode) op
