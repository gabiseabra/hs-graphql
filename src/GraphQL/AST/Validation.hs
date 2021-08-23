{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , RankNTypes
  , RecordWildCards
#-}

module GraphQL.AST.Validation where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser)
import GraphQL.Response (V, Pos(..))
import qualified GraphQL.Response as E
import GraphQL.TypeSystem.Main (OperationType(..))

import Control.Applicative (liftA2)
import Control.Comonad.Cofree (Cofree(..), ComonadCofree(..), unfoldM)
import Control.Monad (join, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.State.Lazy (StateT(..))
import Control.Arrow ((>>>))
import qualified Control.Monad.State.Lazy as ST
import qualified Data.Aeson as JSON
import Data.Function (on)
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Base (TreeF(..))
import Text.Megaparsec (customFailure)
import Data.Monoid (All(..))
import Data.Functor.Foldable (Recursive(..), Base)
import Data.Functor.Identity (Identity)

{-
validateDocument :: Maybe Name -> JSON.Object -> RootNodes'RAW -> V (HashMap Name Fragment, DocumentF SelectionSet)
validateDocument opName input (frags, ops) = do
  Operation'RAW { .. } <- getOperation opName ops
  let
    fn = validateSelectionSet input $ _opVariables
  (,) <$> traverse (traverse fn) frags
      <*> (mkDocument _opPos _opType _opName =<< traverse fn _opSelection)

mkDocument :: Pos -> OperationType -> Maybe Name -> NonEmpty a -> V (DocumentF a)
mkDocument _   QUERY        name sel       = pure $ Query name sel
mkDocument _   MUTATION     name sel       = pure $ Mutation name sel
mkDocument _   SUBSCRIPTION name (sel:|[]) = pure $ Subscription name sel
mkDocument pos _ _ _ = E.validationError [pos] "Subscription operation must have only one root field"

getOperation :: Maybe Name -> NonEmpty Operation'RAW -> V Operation'RAW
getOperation Nothing (op:|[]) = pure op
getOperation Nothing _ = E.validationError [] "No operation name provided for document with multiple operations"
getOperation (Just opName) ops =
  case List.find ((== Just opName) . _opName) (NE.toList ops) of
    Nothing -> E.validationError [] $ "Operation " <> opName <> " is not defined"
    Just op -> pure op

collectFields
  :: HashMap Name Fragment
  -> DocumentF SelectionSet
  -> V Document
collectFields frags doc = do
  (sel, visited) <- runStateT (traverseAccum (eraseFragments frags) $ opSelection doc) mempty
  let unused = Map.filterWithKey (\k _ -> Set.notMember k visited) frags
  if length visited == length frags
    then pure $ setSelection doc sel
    else E.validationError (fmap fragPos $ Map.elems unused)
          $ "Document has unused fragments: "
          <> Text.intercalate ", " (Map.keys unused)

setSelection :: DocumentF a -> NonEmpty b -> DocumentF b
setSelection (Query        name _) as     = Query        name as
setSelection (Mutation     name _) as     = Mutation     name as
setSelection (Subscription name _) (a:|_) = Subscription name a

eraseFragments
  :: HashMap Name (FragmentF (SelectionNode a))
  -> SelectionNode a
  -> StateT (Set Name) V (NonEmpty (Cofree (TreeF a) Pos))
eraseFragments frags (pos :< Node a as) =
  pure . (pos :<) . NodeF a <$> traverseAccum (fmap NE.toList . eraseFragments frags) as
eraseFragments frags (_ :< InlineFragment _ as) =
  traverseAccum (eraseFragments frags) as
eraseFragments frags (pos :< FragmentSpread k) = do
  visited <- ST.get
  if Set.member k visited
    then lift $ E.validationError [pos] $ "Cycle in fragment " <> k
    else case Map.lookup k frags of
      Nothing   -> lift $ E.validationError [pos] $ "Fragment " <> k <> " is not defined"
      Just frag -> do
        ST.put (Set.insert k visited)
        traverseAccum (eraseFragments frags) $ fragSelection frag

validateSelectionSet :: HashMap Name (Variable JSON.Value) -> SelectionNode'RAW -> V SelectionSet
validateSelectionSet vars = hoistM $ bitraverse (validateField vars) pure

applyInput :: HashMap k v -> HashMap k (Variable (Maybe v)) -> V (HashMap k (Variable v))
applyInput = Map.filter (/= JSON.Null) >>> \input -> Map.traverseWithKey (go . (`Map.lookup` input))
  where
    go (Just v) var = pure $ var { varValue = v }
    go Nothing var
      | Just v <- varValue var = pure $ var { varValue = v }
      | isNullable var = pure $ var { varValue = JSON.Null }
      | otherwise = validationError [varPos var] $ "Required variable $" <> k <> " is missing from input"

validateField :: JSON.Object -> HashMap Name Variable'RAW -> Field'RAW -> V Field
validateField input vars field = do
  args <- traverse (eraseVars input vars) (fieldArgs field)
  pure $ Field (fieldType field) (fieldAlias field) (fieldName field) args

eraseVars :: JSON.Object -> HashMap Name Variable'RAW -> Value'RAW -> V Value
eraseVars _ _    (pos :< NullVal)       = pure $ (pos, Nothing) :< NullVal
eraseVars _ _    (pos :< StrVal val)    = pure $ (pos, Nothing) :< (StrVal val)
eraseVars _ _    (pos :< IntVal val)    = pure $ (pos, Nothing) :< (IntVal val)
eraseVars _ _    (pos :< DoubleVal val) = pure $ (pos, Nothing) :< (DoubleVal val)
eraseVars _ _    (pos :< BoolVal val)   = pure $ (pos, Nothing) :< (BoolVal val)
eraseVars _ _    (pos :< EnumVal val)   = pure $ (pos, Nothing) :< (EnumVal val)
eraseVars i vars (pos :< ListVal val)   = ((pos, Nothing) :<) . ListVal <$> mapM (eraseVars i vars) val
eraseVars i vars (pos :< ObjectVal val) = ((pos, Nothing) :<) . ObjectVal <$> mapM (eraseVars i vars) val
eraseVars i vars (pos :< Var k)         = case (Map.lookup k vars, Map.lookup k i) of
  (Nothing, _) ->
    E.validationError [pos] $ "Variable $" <> k <> " is not defined"
  (Just var, Nothing) ->
    fmap (att (pos, Just $ _varTypeDef var)) . eraseVars i vars =<< getDefaultValue k var
  (Just var, Just val) ->
    pure $ (pos, Just $ _varTypeDef var) :< Var val
-}

validateDocumentP :: ([(Name, Fragment a)], [Operation a]) -> Parser (Document a)
validateDocumentP (frags, ops) = Document <$> validateFragmentsP frags <*> validateOperationsP ops

-- Validates that each document either has only one operation or all named operations
validateOperationsP :: [Operation a] -> Parser (EitherF Identity (HashMap Name) (Operation a))
validateOperationsP [] = parseErrorP [] "Expected at least one root operation, found none"
validateOperationsP (op:[]) = pure $ LeftF $ pure op
validateOperationsP ops = RightF <$> (sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) =<< mapM go ops)
  where
    go op | Just name <- opName op = pure (name, pure op)
          | otherwise = validationErrorP [opPos op] $ "Unnamed operation in document with multiple operations"
    onDupe k op0 op1 = validationErrorP [opPos op0, opPos op1] $ "Duplicated operation name " <> k

-- Validates that fragment names don't clash
validateFragmentsP :: [(Name, Fragment a)] -> Parser (HashMap Name (Fragment a))
validateFragmentsP = sequence . HashMap.fromListWithKey (liftJoin2 . onDupe) . fmap (second pure)
  where
    onDupe k f0 f1 = validationErrorP [fragPos f0, fragPos f1] $ "Duplicated fragment name " <> k

-- Validates that a default value in a variable might match its type definitions
validateVarP :: Variable (Maybe ConstValue) -> Parser (Variable (Maybe JSON.Value))
validateVarP var@(Variable _ _   Nothing          ) = pure $ var { varValue = Nothing }
validateVarP var@(Variable _ def (Just (pos:<val))) =
  if typeMatches def val
    then pure $ var { varValue = Just $ constValToJSON val }
    else parseErrorP [pos]
          $ "Expected "
          <> Text.pack (show def)
          <> ", found "
          <> Text.pack (showConstVal val)

parseErrorP :: [Pos] -> Text -> Parser a
parseErrorP pos msg = customFailure $ E.ParseError pos msg

validationErrorP :: [Pos] -> Text -> Parser a
validationErrorP pos msg = customFailure $ E.ValidationError pos msg

traverseAccum :: (Monad m, Traversable f, Monad f) => (a -> m (f b)) -> f a -> m (f b)
traverseAccum f = pure . join <=< traverse f

att :: a -> Cofree f a -> Cofree f a
att a (_:<f) = (a:<f)

hoistCofreeM :: (Traversable f, Monad m) => (forall x . f x -> m (g x)) -> Cofree f a -> m (Cofree g a)
hoistCofreeM f (x:<y) = (x:<) <$> (f =<< traverse (hoistCofreeM f) y)

-- hoistCofreeBiA :: (Bitraversable t, Applicative f) => (a -> f b) -> Cofree (t a) c -> f (Cofree (t b) c)
-- hoistCofreeBiA f = bitraverseCofree f pure

-- bitraverseCofree :: (Bitraversable t, Applicative f) => (a -> f b) -> (c -> f d) -> Cofree (t a) c -> f (Cofree (t b) d)
-- bitraverseCofree f g (a:<e) = (:<) <$> g a <*> (bitraverse f (bitraverseCofree f g) e)

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb = join (liftA2 f ma mb)

isNullable :: TypeDefinition -> Bool
isNullable (NonNullType _) = False
isNullable _               = True

typeMatches :: TypeDefinition -> ConstValueF ConstValue -> Bool
typeMatches (ListType    t) (ListVal as) = getAll $ foldMap (All . typeMatches t . unwrap) as
typeMatches (ListType    t) _            = False
typeMatches (NonNullType t) NullVal      = False
typeMatches (NonNullType t) v            = typeMatches t v
typeMatches _               v            = True
