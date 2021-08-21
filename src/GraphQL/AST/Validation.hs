{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , RankNTypes
  , RecordWildCards
#-}

module GraphQL.AST.Validation
  ( validateVarP
  , validateRootNodesP
  , validateDocument
  , collectFields
  ) where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser)
import GraphQL.Response (V, Pos(..))
import qualified GraphQL.Response as E

import Control.Comonad.Cofree (Cofree(..), unfoldM)
import Control.Monad (join, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.State.Lazy (StateT(..))
import qualified Control.Monad.State.Lazy as ST
import qualified Data.Aeson as JSON
import Data.Function (on)
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Base (TreeF(..))
import Text.Megaparsec (customFailure)

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

validateSelectionSet :: JSON.Object -> HashMap Name Variable'RAW -> SelectionNode'RAW -> V SelectionSet
validateSelectionSet input vars = hoistM $ bitraverse (validateField input' vars) pure
  where input' = Map.filter (/= JSON.Null) input

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

getDefaultValue :: Name -> Variable'RAW -> V Value'RAW
getDefaultValue k var = maybe null pure $ _varDefValue var
  where
    null = if isNullable (_varTypeDef var)
      then pure $ _varPos var :< NullVal
      else E.validationError [_varPos var] $ "Required variable $" <> k <> " is missing from input"

isNullable :: TypeDefinition -> Bool
isNullable (NonNullType _) = False
isNullable _               = True

validateRootNodesP :: ([(Name, Fragment'RAW)], [Operation'RAW]) -> Parser RootNodes'RAW
validateRootNodesP (frags, ops) = (,) <$> validateFragmentsP frags <*> validateOperationsP ops

validateOperationsP :: [Operation'RAW] -> Parser (NonEmpty Operation'RAW)
validateOperationsP [] = parseErrorP [] "Expected at least one root operation, found none"
validateOperationsP (op:[]) = pure (op:|[])
validateOperationsP allOps@(op:ops) = do
  case List.filter ((== Nothing) . _opName) allOps of
    []      -> pure ()
    unnamed -> validationErrorP (fmap _opPos unnamed) $ "Unnamed operations in document with multiple operations"
  case dupesWith ((==) `on` _opName) allOps of
    []    -> pure ()
    dupes ->
      validationErrorP (fmap _opPos dupes)
      $ "Duplicated operation names: "
      <> (Text.intercalate ", " $ foldMap (maybe [] pure . _opName) dupes)
  pure (op:|ops)

validateFragmentsP :: [(Name, Fragment'RAW)] -> Parser (HashMap Name Fragment'RAW)
validateFragmentsP frags =
  if length frags == length fragsMap
    then pure fragsMap
    else parseErrorP locs $ "Duplicated fragment names: " <> Text.intercalate ", " names
  where
    fragsMap = Map.fromList frags
    dupes = dupesWith ((==) `on` fst) frags
    locs  = fmap (fragPos . snd) dupes
    names = fmap fst dupes

validateVarP :: Name -> Variable'RAW -> Parser Variable'RAW
validateVarP k var = var <$ maybe (pure ()) fun (_varDefValue var)
  where fun = validateVarP' k (_varTypeDef var)

validateVarP' :: Name -> TypeDefinition -> Value'RAW -> Parser ()
validateVarP' k (ListType t) (_ :< ListVal as) = foldMap (validateVarP' k t) as
validateVarP' k (ListType t) (p :< _) = parseErrorP [p] $ "Non list value in list variable $" <> k
validateVarP' k (NonNullType t) (p :< NullVal) = parseErrorP [p] $ "Null default value in required variable $" <> k
validateVarP' k (NonNullType t) v = validateVarP' k t v
validateVarP' k _ v@(p :< Var k') =
  if k /= k'
    then pure ()
    else parseErrorP [p] $ "Self reference in default value of $" <> k
validateVarP' _ _ v = pure ()

parseErrorP :: [Pos] -> Text -> Parser a
parseErrorP pos msg = customFailure $ E.ParseError pos msg

validationErrorP :: [Pos] -> Text -> Parser a
validationErrorP pos msg = customFailure $ E.ValidationError pos msg

dupesWith :: (a -> a -> Bool) -> [a] -> [a]
dupesWith _ [] = []
dupesWith f (x:xs) = filter (f x) xs ++ dupesWith f xs

traverseAccum :: (Monad m, Traversable f, Monad f) => (a -> m (f b)) -> f a -> m (f b)
traverseAccum f = pure . join <=< traverse f

att :: a -> Cofree f a -> Cofree f a
att a (_:<f) = (a:<f)

hoistM :: (Traversable f, Monad m) => (forall x . f x -> m (g x)) -> Cofree f a -> m (Cofree g a)
hoistM f (x :< y) = (x:<) <$> (f =<< traverse (hoistM f) y)
