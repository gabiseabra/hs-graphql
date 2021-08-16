{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , RankNTypes
#-}

module GraphQL.AST.Validation
  ( validateVarP
  , validateRootNodesP
  , validateDocument
  , collectFields
  ) where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser)
import GraphQL.Error (V)
import qualified GraphQL.Error as E

import Control.Comonad.Cofree (Cofree(..), unfoldM)
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

validateDocument :: Maybe Name -> Input -> RootNodes'RAW -> V (HashMap Name Fragment, Document SelectionSet)
validateDocument opName input (frags, ops) = do
  op <- getOperation opName ops
  let fn = hoistM $ bitraverse (validateField (Map.filter (/= JSON.Null) input) (_opVariables op)) pure
  sel' <- traverse fn $ _opSelection op
  frags' <- traverse (traverse fn) frags
  pure (frags', Document (_opType op) (_opName op) sel')

getOperation :: Maybe Name -> NonEmpty Operation'RAW -> V Operation'RAW
getOperation Nothing (op:|[]) = pure op
getOperation Nothing _ = E.validationError [] "No operation name provided for document with multiple operations"
getOperation (Just opName) ops =
  case List.find ((== Just opName) . _opName) (NE.toList ops) of
    Nothing -> E.validationError [] $ "Operation " <> opName <> " is not defined"
    Just op -> pure op

collectFields
  :: HashMap Name Fragment
  -> Document SelectionSet
  -> V (Document SelectionTree)
collectFields frags doc = do
  (sel, visited) <- runStateT (foldMapM (eraseFragments frags) $ opSelection doc) mempty
  let unused = Map.filterWithKey (\k _ -> Set.notMember k visited) frags
  if length visited == length frags
    then pure $ doc { opSelection = sel }
    else E.validationError (fmap fragPos $ Map.elems unused)
          $ "Document has unused fragments: "
          <> Text.intercalate ", " (Map.keys unused)

eraseFragments
  :: HashMap Name (FragmentF (SelectionNode a))
  -> SelectionNode a
  -> StateT (Set Name) V [Cofree (TreeF a) Pos]
eraseFragments frags (pos :< Node a as) =
  pure . (pos :<) . NodeF a <$> foldMapM (eraseFragments frags) as
eraseFragments frags (_ :< InlineFragment _ as) =
  foldMapM (eraseFragments frags) as
eraseFragments frags (pos :< FragmentSpread k) = do
  visited <- ST.get
  if Set.member k visited
    then lift $ E.validationError [pos] $ "Cycle in fragment " <> k
    else case Map.lookup k frags of
      Nothing   -> lift $ E.validationError [pos] $ "Fragment " <> k <> " is not defined"
      Just frag -> do
        ST.put (Set.insert k visited)
        foldMapM (eraseFragments frags) $ fragSelection frag

validateField :: Input -> HashMap Name Variable'RAW -> Field'RAW -> V Field
validateField input vars field = do
  args <- traverse (eraseVars input vars) (fieldArgs field)
  pure $ Field (fieldType field) (fieldAlias field) (fieldName field) args

eraseVars :: Input -> HashMap Name Variable'RAW -> Value'RAW -> V Value
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

foldMapM :: Applicative m => Monoid b => Foldable f => (a -> m b) -> f a -> m b
foldMapM f = foldr (\a bs -> mappend <$> f a <*> bs) (pure mempty)

dupesWith :: (a -> a -> Bool) -> [a] -> [a]
dupesWith _ [] = []
dupesWith f (x:xs) = filter (f x) xs ++ dupesWith f xs

att :: a -> Cofree f a -> Cofree f a
att a (_:<f) = (a:<f)

hoistM :: (Traversable f, Monad m) => (forall x . f x -> m (g x)) -> Cofree f a -> m (Cofree g a)
hoistM f (x :< y) = (x:<) <$> (f =<< traverse (hoistM f) y)
