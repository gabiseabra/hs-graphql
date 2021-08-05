{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , RankNTypes
  , ScopedTypeVariables
#-}

module GraphQL.AST.Validation
  ( validateVarP
  , validateRootNodesP
  , validateDocument
  ) where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser)
import GraphQL.Error (V)
import qualified GraphQL.Error as E

import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Trans (lift)
import Control.Monad.State.Lazy (StateT(..))
import qualified Control.Monad.State.Lazy as ST
import qualified Data.Aeson as JSON
import Data.Bifunctor (first, second)
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Base (TreeF(..))
import Text.Megaparsec (customFailure)

validateDocument :: Maybe Name -> Input -> RootNodes'RAW -> V Document
validateDocument opName input (frags, ops) = do
  let input' = Map.filter (/= JSON.Null) input
  (opType, opName', vars, selection, _) <- validateOperation opName ops
  Document opType opName' <$> eraseSelectionWith (validateField input' vars) frags selection

validateOperation :: Maybe Name -> NonEmpty Operation'RAW -> V Operation'RAW
validateOperation Nothing (op:|[]) = pure op
validateOperation Nothing _ = E.validationError [] "No operation name provided for document with multiple operations"
validateOperation (Just opName) ops =
  case List.find ((== Just opName) . getName) (NE.toList ops) of
    Nothing -> E.validationError [] $ "Operation " <> opName <> " is not defined"
    Just op -> pure op
  where getName (_,a,_,_,_) = a

validateField :: Input -> HashMap Name Variable'RAW -> Field'RAW -> V Field
validateField input vars (ty, alias, name, val) = Field ty alias name <$> traverse (eraseVars input vars) val

eraseSelectionWith :: forall a
  . (Field'RAW -> V a)
  -> HashMap Name Fragment'RAW
  -> [SelectionNode'RAW]
  -> V [Cofree (TreeF a) Pos]
eraseSelectionWith f frags s = do
  (s', visited) <- runStateT (foldMapM (eraseFragmentsWith f frags) s) mempty
  let unusedFrags = Map.filterWithKey (\k _ -> Set.notMember k visited) frags
  if length visited == length frags
    then pure s'
    else E.validationError (fmap trd $ Map.elems unusedFrags)
          $ "Document has unused fragments: "
          <> Text.intercalate ", " (Map.keys unusedFrags)

eraseFragmentsWith :: forall a
  .  (Field'RAW -> V a)
  -> HashMap Name Fragment'RAW
  -> SelectionNode'RAW
  -> StateT (Set Name) V [Cofree (TreeF a) Pos]
eraseFragmentsWith f frags (pos :< Node a as)
  = pure . (pos :<) <$> (NodeF <$> lift (f a) <*> foldMapM (eraseFragmentsWith f frags) as)
eraseFragmentsWith f frags (pos :< InlineFragment t as)
  = foldMapM (eraseFragmentsWith f frags) as
eraseFragmentsWith f frags (pos:<FragmentSpread t) = do
  visited <- ST.get
  if Set.member t visited
    then lift $ E.validationError [pos] $ "Cycle in fragment " <> t
    else  case Map.lookup t frags of
      Nothing         -> lift $ E.validationError [pos] $ "Fragment " <> t <> " is not defined"
      Just (_, as, _) -> do
        ST.put (Set.insert t visited)
        foldMapM (eraseFragmentsWith f frags) as

eraseVars :: Input -> HashMap Name Variable'RAW -> Value'RAW -> V Value
eraseVars _ _    (pos :< NullVal)       = pure $ (pos, Nothing) :< NullVal
eraseVars _ _    (pos :< StrVal val)    = pure $ (pos, Nothing) :< (StrVal val)
eraseVars _ _    (pos :< IntVal val)    = pure $ (pos, Nothing) :< (IntVal val)
eraseVars _ _    (pos :< DoubleVal val) = pure $ (pos, Nothing) :< (DoubleVal val)
eraseVars _ _    (pos :< BoolVal val)   = pure $ (pos, Nothing) :< (BoolVal val)
eraseVars _ _    (pos :< EnumVal val)   = pure $ (pos, Nothing) :< (EnumVal val)
eraseVars i vars (pos :< ListVal val)   = ((pos, Nothing) :<) . ListVal <$> mapM (eraseVars i vars) val
eraseVars i vars (pos :< ObjectVal val) = ((pos, Nothing) :<) . ObjectVal <$> mapM (eraseVars i vars) val
eraseVars i vars (pos :< Var k)      = case (Map.lookup k vars, Map.lookup k i) of
  (Nothing, _) -> E.validationError [pos] $ "Variable $" <> k <> " is not defined"
  (Just var@(ty, _, varPos), Nothing) ->
    let err = E.validationError [varPos] $ "Required variable $" <> k <> " is missing from input"
    in maybe err (fmap (att (pos, Just ty)) . eraseVars i vars) $ defValue var
  (Just (ty, _, _), Just val) -> pure $ (pos, Just ty) :< Var val

validateRootNodesP :: ([(Name, Fragment'RAW)], [Operation'RAW]) -> Parser RootNodes'RAW
validateRootNodesP (frags, ops) = (,) <$> validateFragmentsP frags <*> validateOperationsP ops

validateOperationsP :: [Operation'RAW] -> Parser (NonEmpty Operation'RAW)
validateOperationsP [] = parseErrorP [] "Expected at least one root operation, found none"
validateOperationsP (op:[]) = pure (op:|[])
validateOperationsP allOps@(op:ops) = do
  case List.filter ((== Nothing) . getName) allOps of
    []      -> pure ()
    unnamed -> validationErrorP (fmap getPos unnamed) $ "Unnamed operations in document with multiple operations"
  case dupesWith (\a b -> getName a == getName b) allOps of
    []    -> pure ()
    dupes ->
      validationErrorP (fmap getPos dupes)
      $ "Duplicated operation names: "
      <> (Text.intercalate ", " $ foldMap (maybe [] pure . getName) dupes)
  pure (op:|ops)
  where
    getName (_,a,_,_,_) = a
    getPos (_,_,_,_,a) = a

validateFragmentsP :: [(Name, Fragment'RAW)] -> Parser (HashMap Name Fragment'RAW)
validateFragmentsP frags =
  if length frags == length fragsMap
    then pure fragsMap
    else parseErrorP locs $ "Duplicated fragment names: " <> Text.intercalate ", " names
  where
    fragsMap = Map.fromList frags
    dupes = dupesWith (\a b -> fst a == fst b) frags
    locs  = fmap (pos . snd) dupes
    names = fmap fst dupes
    pos (_,_,a) = a

validateVarP :: Name -> Variable'RAW -> Parser Variable'RAW
validateVarP k var@(t, Just v, _) = var <$ validateVarP' k t v
validateVarP k var@(t, Nothing, _) = pure var

validateVarP' :: Name -> TypeDefinition -> Value'RAW -> Parser ()
validateVarP' k (ListType t) (_ :< ListVal as) = foldMap (validateVarP' k t) as
validateVarP' k (ListType t) (pos :< _) = parseErrorP [pos] $ "Non list value in list variable $" <> k
validateVarP' k (NonNullType t) (pos :< NullVal) = parseErrorP [pos] $ "Null default value in required variable $" <> k
validateVarP' k (NonNullType t) v = validateVarP' k t v
validateVarP' k _ v@(pos :< Var k') =
  if k /= k'
    then pure ()
    else parseErrorP [pos] $ "Self reference in default value of $" <> k
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

uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = Set.toList . Set.fromList

isNullable :: Variable'RAW -> Bool
isNullable (NonNullType _, _, _) = False
isNullable _                     = True

defValue :: Variable'RAW -> Maybe Value'RAW
defValue (_, Just a, _)              = Just a
defValue (NonNullType _, Nothing, _) = Nothing
defValue (_, _, pos)                 = Just $ pos:<NullVal

att :: a -> Cofree f a -> Cofree f a
att a (_:<f) = (a:<f)

trd (_,_,a) = a
