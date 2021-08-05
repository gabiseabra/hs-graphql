{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , RankNTypes
  , ScopedTypeVariables
#-}

module GraphQL.AST.Validation where

import GraphQL.AST.Document
import GraphQL.AST.Lexer (Parser)
import GraphQL.Error (V)
import qualified GraphQL.Error as E

import Control.Comonad.Cofree (Cofree(..))
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
validateVarP' k (ListType t) (loc :< _) = parseError [loc] $ "Non list value in list variable $" <> k
validateVarP' k (NonNullType t) (loc :< NullVal) = parseError [loc] $ "Null default value in required variable $" <> k
validateVarP' k (NonNullType t) v = validateVarP' k t v
validateVarP' k _ v@(loc :< VarVal k') =
  if k /= k'
    then pure ()
    else parseError [loc] $ "Self reference in default value of $" <> k
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
    locs  = fmap (loc . snd) dupes
    names = uniq $ fmap fst dupes
    loc (_,_,a) = a

eraseVars :: Input -> HashMap Name Variable'RAW -> Value'RAW -> V Value
eraseVars _ _    (loc :< NullVal)       = pure $ (loc, Nothing) :< NullVal
eraseVars _ _    (loc :< StrVal val)    = pure $ (loc, Nothing) :< (StrVal val)
eraseVars _ _    (loc :< IntVal val)    = pure $ (loc, Nothing) :< (IntVal val)
eraseVars _ _    (loc :< DoubleVal val) = pure $ (loc, Nothing) :< (DoubleVal val)
eraseVars _ _    (loc :< BoolVal val)   = pure $ (loc, Nothing) :< (BoolVal val)
eraseVars _ _    (loc :< EnumVal val)   = pure $ (loc, Nothing) :< (EnumVal val)
eraseVars i vars (loc :< ListVal val)   = ((loc, Nothing) :<) . ListVal <$> mapM (eraseVars i vars) val
eraseVars i vars (loc :< ObjectVal val) = ((loc, Nothing) :<) . ObjectVal <$> mapM (eraseVars i vars) val
eraseVars i vars (loc :< VarVal k)      = case (Map.lookup k vars, Map.lookup k i) of
  (Nothing, _) -> validationError [loc] $ "Variable $" <> k <> " is not defined"
  (Just var@(ty, _, _), Nothing) ->
    let err = validationError [loc] $ "Required variable $" <> k <> " is missing from input"
    in maybe err (fmap (att (loc, Just ty)) . eraseVars i vars) $ defValue var
  (Just (ty, _, _), Just val) -> pure $ (loc, Just ty) :< VarVal val

eraseFragmentsWith :: forall a
  .  (Field'RAW -> V a)
  -> HashMap Name Fragment'RAW
  -> Set Name
  -> SelectionNode'RAW
  -> V (Set Name, [Cofree (TreeF a) Location])
eraseFragmentsWith f frags visited (loc:<Node a as) = do
  a'              <- f a
  (visited', as') <- foldMapM (eraseFragmentsWith f frags visited) as
  pure (visited', [loc :< NodeF a' as'])
eraseFragmentsWith f frags visited (loc:<InlineFragment t as) =
  foldMapM (eraseFragmentsWith f frags visited) as
eraseFragmentsWith f frags visited (loc:<FragmentSpread t) = do
  if Set.member t visited
    then validationError [loc] $ "Cycle in fragment " <> t
    else  case Map.lookup t frags of
      Nothing         -> validationError [loc] $ "Fragment " <> t <> " is not defined"
      Just (_, as, _) -> foldMapM (eraseFragmentsWith f frags $ Set.insert t visited) as

eraseSelectionWith :: forall a
  . (Field'RAW -> V a)
  -> HashMap Name Fragment'RAW
  -> [SelectionNode'RAW]
  -> V [Cofree (TreeF a) Location]
eraseSelectionWith f frags s = do
  (visited, s') <- foldMapM (eraseFragmentsWith f frags mempty) s
  let unusedFrags = Map.filterWithKey (\k _ -> Set.notMember k visited) frags
  if length visited == length frags
    then pure s'
    else validationError (fmap trd $ Map.elems unusedFrags)
          $ "Document has unused fragments: "
          <> Text.intercalate ", " (Map.keys unusedFrags)

parseError :: [Location] -> Text -> Parser a
parseError loc msg = either customFailure pure $ E.parseError loc msg

validationError = E.validationError

foldMapM :: Applicative m => Monoid b => Foldable f => (a -> m b) -> f a -> m b
foldMapM f = foldr (\a bs -> mappend <$> f a <*> bs) (pure mempty)

dupesWith :: (a -> a -> Bool) -> [a] -> [a]
dupesWith f (x : xs) = filter (f x) xs ++ dupesWith f xs

uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = Set.toList . Set.fromList

isNullable :: Variable'RAW -> Bool
isNullable (NonNullType _, _, _) = False
isNullable _                     = True

defValue :: Variable'RAW -> Maybe Value'RAW
defValue (_, Just a, _)              = Just a
defValue (NonNullType _, Nothing, _) = Nothing
defValue (_, _, loc)                 = Just $ loc:<NullVal

att :: a -> Cofree f a -> Cofree f a
att a (_:<f) = (a:<f)

trd (_,_,a) = a
