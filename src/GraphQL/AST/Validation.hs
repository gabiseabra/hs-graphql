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
import qualified Data.HashMap.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor.Base (TreeF(..))
import Text.Megaparsec (customFailure)

validateVarP' :: Name -> TypeDefinition -> Value'RAW -> Parser ()
validateVarP' k (ListType t) (_ :< ListVal as) = foldMap (validateVarP' k t) as
validateVarP' k (ListType t) (pos :< _) = parseError [pos] $ "Non list value in list variable $" <> k
validateVarP' k (NonNullType t) (pos :< NullVal) = parseError [pos] $ "Null default value in required variable $" <> k
validateVarP' k (NonNullType t) v = validateVarP' k t v
validateVarP' k _ v@(pos :< Var k') =
  if k /= k'
    then pure ()
    else parseError [pos] $ "Self reference in default value of $" <> k
validateVarP' _ _ v = pure ()

validateVarP :: Name -> Variable'RAW -> Parser Variable'RAW
validateVarP k var@(t, Just v, _) = var <$ validateVarP' k t v
validateVarP k var@(t, Nothing, _) = pure var

validateRootNodesP :: ([Operation'RAW], [(Name, Fragment'RAW)]) -> Parser RootNodes'RAW
validateRootNodesP ([], _) = parseError [] "Expected at least one root operation"
validateRootNodesP (ops@(_:(_:_)), _) = parseError [] $ "Expected at most one root operation, got " <> n
  where n = Text.pack $ show $ length ops
validateRootNodesP ((op:_), frags) =
  if length frags == length fragsMap
    then pure (op, fragsMap)
    else parseError locs $ "Duplicated fragment names: " <> Text.intercalate ", " names
  where
    fragsMap = Map.fromList frags
    dupes = dupesWith (\a b -> fst a == fst b) frags
    locs  = fmap (pos . snd) dupes
    names = uniq $ fmap fst dupes
    pos (_,_,a) = a

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
  (Nothing, _) -> validationError [pos] $ "Variable $" <> k <> " is not defined"
  (Just var@(ty, _, varPos), Nothing) ->
    let err = validationError [varPos] $ "Required variable $" <> k <> " is missing from input"
    in maybe err (fmap (att (pos, Just ty)) . eraseVars i vars) $ defValue var
  (Just (ty, _, _), Just val) -> pure $ (pos, Just ty) :< Var val

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
    then lift $ validationError [pos] $ "Cycle in fragment " <> t
    else  case Map.lookup t frags of
      Nothing         -> lift $ validationError [pos] $ "Fragment " <> t <> " is not defined"
      Just (_, as, _) -> do
        ST.put (Set.insert t visited)
        foldMapM (eraseFragmentsWith f frags) as

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
    else validationError (fmap trd $ Map.elems unusedFrags)
          $ "Document has unused fragments: "
          <> Text.intercalate ", " (Map.keys unusedFrags)

validateField :: Input -> HashMap Name Variable'RAW -> Field'RAW -> V Field
validateField input vars (ty, alias, name, val) = Field ty alias name <$> traverse (eraseVars input vars) val

validateDocument :: Input -> RootNodes'RAW -> V Document
validateDocument input ((opType, opName, vars, selection, _), frags) =
  let input' = Map.filter (/= JSON.Null) input
  in Document opType opName <$> eraseSelectionWith (validateField input' vars) frags selection

parseError :: [Pos] -> Text -> Parser a
parseError pos msg = customFailure $ E.ParseError pos msg

validationError = E.validationError

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
