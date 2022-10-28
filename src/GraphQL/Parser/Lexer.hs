{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.Parser.Lexer
  ( Parser
  , whiteSpace
  , lineTerminator
  , ignoredTokens
  , sc
  , lexeme
  , symbol
  , foldE
  , manyE
  , optional_
  , mkPos
  , getPos
  , withPos
  , (<@>)
  , braces
  , parens
  , brackets
  , args
  , argsE
  , name
  , varName
  , directiveName
  , enumVal
  , boolVal
  , intVal
  , doubleVal
  , stringVal
  , syntaxErrorP
  , validationErrorP
  ) where

import           Control.Applicative            ( (<|>)
                                                , empty
                                                )
import           Control.Monad                  ( void )
import           Data.Char                      ( chr
                                                , digitToInt
                                                )
import           Data.Foldable                  ( foldl'
                                                , foldr
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import qualified Data.List                     as List
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Scientific                ( toRealFloat )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void                      ( Void )
import qualified GraphQL.Response              as E
import           Text.Megaparsec                ( MonadParsec(..)
                                                , ParseError(..)
                                                , Parsec
                                                , between
                                                , choice
                                                , chunkToTokens
                                                , customFailure
                                                , getSourcePos
                                                , label
                                                , many
                                                , manyTill
                                                , manyTill_
                                                , notFollowedBy
                                                , oneOf
                                                , optional
                                                , satisfy
                                                , some
                                                , takeP
                                                , tokensToChunk
                                                , try
                                                )
import           Text.Megaparsec.Char           ( char
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Text.Megaparsec.Pos           as P

type Parser = Parsec E.GraphQLError Text

upper, lower, alpha, ws, num :: String
upper = "_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "_acbcdefghijklmnopqrstuvwxyz"
alpha = upper <> lower
ws = "\x0020\x0009"
num = ['0' .. '9']

upperChar, lowerChar, alphaChar, alphaNumChar :: Parser Char
upperChar = satisfy (`elem` upper)
lowerChar = satisfy (`elem` lower)
alphaChar = satisfy (`elem` alpha)
alphaNumChar = satisfy (`elem` (alpha <> num))

-- https://spec.graphql.org/June2018/#sec-White-Space
whiteSpace :: Parser ()
whiteSpace = label "WhiteSpace" $ void $ satisfy (`elem` ws)
-- http://spec.graphql.org/June2018/#sec-Line-Terminators
lineTerminator :: Parser ()
lineTerminator = label "LineTerminator" $ void $ choice
  [char '\x000A', char '\x000D' <* optional (char '\x000A')]
-- http://spec.graphql.org/June2018/#sec-Insignificant-Commas
insignificantComma :: Parser ()
insignificantComma = label "InsignificantComma" $ void $ char ','
-- http://spec.graphql.org/June2018/#sec-Unicode
unicodeBOM :: Parser ()
unicodeBOM = label "UnicodeBOM" $ void $ char '\xFEFF'
-- http://spec.graphql.org/June2018/#sec-Source-Text.Ignored-Tokens
ignoredTokens :: Parser ()
ignoredTokens =
  choice [whiteSpace, lineTerminator, insignificantComma, unicodeBOM]

-- http://spec.graphql.org/June2018/#sec-Comments
sc :: Parser ()
sc = L.space ignoredTokens (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

optional_ :: Monoid m => Parser m -> Parser m
optional_ = fmap (fromMaybe mempty) . optional

between_ :: Parser sep -> Parser a -> Parser a
between_ a = between a a

mkPos :: P.SourcePos -> E.Pos
mkPos p = E.Pos (P.unPos $ P.sourceLine p) (P.unPos $ P.sourceColumn p)

getPos :: Parser E.Pos
getPos = mkPos <$> getSourcePos

withPos :: (E.Pos -> a -> b) -> Parser a -> Parser b
withPos f p = f <$> getPos <*> p

(<@>) :: Parser a -> (E.Pos -> a -> b) -> Parser b
(<@>) = flip withPos

foldWithRecovery
  :: MonadParsec e s m => (a -> ParseError s e -> m a) -> (a -> m a) -> a -> m a
foldWithRecovery r f = let go a = withRecovery (r a) (go =<< f a) in go

foldE :: MonadParsec e s m => (a -> m a) -> a -> m a
foldE = foldWithRecovery recoverFromTrivialError

manyE :: MonadParsec e s m => ([a] -> m a) -> m [a]
manyE f = foldE (\as -> (: as) <$> f as) []

recoverFromTrivialError a TrivialError{} = pure a
recoverFromTrivialError _ e              = parseError e

braces, parens, brackets :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

argsE'
  :: (Text -> a -> Parser b)
  -> (forall a . Parser a -> Parser a)
  -> Parser Text
  -> Parser a
  -> Parser [(Text, b)]
argsE' f p pK pV = p $ manyE $ \kv -> do
  pos <- getPos
  k   <- pK <* symbol ":"
  if List.any ((== k) . fst) kv
    then syntaxErrorP (Just [pos]) $ "Duplicated argument " <> k
    else (,) <$> pure k <*> (f k =<< pV)

argsE
  :: (Text -> a -> Parser b)
  -> (forall a . Parser a -> Parser a)
  -> Parser Text
  -> Parser a
  -> Parser (HashMap Text b)
argsE f p pK pV = Map.fromList <$> argsE' f p pK pV

args
  :: (forall a . Parser a -> Parser a)
  -> Parser Text
  -> Parser a
  -> Parser (HashMap Text a)
args = argsE (const pure)

-- https://spec.graphql.org/June2018/#sec-Names
name :: Parser Text
name = label "Name" $ lexeme $ do
  a  <- alphaChar
  as <- many alphaNumChar
  pure $ Text.pack (a : as)

varName :: Parser Text
varName = label "VariableName" $ lexeme $ char '$' *> name

directiveName :: Parser Text
directiveName = label "DirectiveName" $ lexeme $ char '@' *> name

-- http://spec.graphql.org/June2018/#EnumValue
enumVal :: Parser Text
enumVal = label "EnumVal" $ lexeme $ Text.pack <$> some upperChar

-- http://spec.graphql.org/June2018/#BooleanValue
boolVal :: Parser Bool
boolVal = label "BoolVal" $ lexeme $ choice
  [True <$ string "true", False <$ string "false"]

signed :: Num a => Parser a -> Parser a
signed = L.signed empty

-- http://spec.graphql.org/June2018/#sec-Int-Value
intVal :: Parser Int
intVal =
  label "IntVal" $ lexeme $ signed L.decimal <* notFollowedBy (symbol ".")

-- http://spec.graphql.org/June2018/#sec-Float-Value
doubleVal :: Parser Double
doubleVal = label "DoubleVal" $ lexeme $ choice
  [toRealFloat <$> signed L.scientific, signed L.float]

-- http://spec.graphql.org/June2018/#SourceCharacter
sourceChar =
  '\x0009' : '\x000A' : '\x000D' : ['\x0020' .. '\xFFFF'] `except` "\""

except :: String -> String -> String
except = foldr List.delete

-- http://spec.graphql.org/June2018/#EscapedUnicode
-- http://spec.graphql.org/June2018/#EscapedCharacter
-- https://www.caraus.tech/projects/pub-graphql/repository/23/revisions/master/entry/src/Language/GraphQL/AST/Lexer.hs#L204
escapeSequence :: Parser Char
escapeSequence = do
  void $ char '\\'
  escaped <- oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u']
  case escaped of
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'u' ->
      chr . foldl' step 0 . chunkToTokens (Proxy @Text) <$> takeP Nothing 4
    _ -> return escaped
  where step accumulator = (accumulator * 16 +) . digitToInt

sc' :: Parser ()
sc' = L.space lineTerminator empty empty

symbol' :: Text -> Parser Text
symbol' = L.symbol sc'

lineString :: Parser Text
lineString = label "InlineString" $ lexeme $ Text.pack <$> between_
  delim
  (many chars)
 where
  delim = symbol' "\""
  chars = choice [escapeSequence, satisfy (`elem` (sourceChar `except` "\n"))]

blockString :: Parser Text
blockString =
  label "BlockString" $ lexeme $ delim >> Text.pack <$> manyTill chars delim
 where
  delim = symbol' "\"\"\""
  chars = choice [escapeSequence, satisfy (`elem` sourceChar)]

stringVal :: Parser Text
stringVal = label "StringVal" $ choice [blockString, lineString] <* sc

syntaxErrorP :: Maybe [E.Pos] -> Text -> Parser a
syntaxErrorP pos msg =
  customFailure $ E.GraphQLError E.SYNTAX_ERROR pos Nothing msg

validationErrorP :: [E.Pos] -> Text -> Parser a
validationErrorP pos msg =
  customFailure $ E.GraphQLError E.VALIDATION_ERROR (Just pos) Nothing msg
