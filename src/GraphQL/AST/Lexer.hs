{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , RankNTypes
#-}

module GraphQL.AST.Lexer
  ( Parser
  , whiteSpace
  , lineTerminator
  , ignoredTokens
  , sc
  , lexeme
  , symbol
  , braces
  , parens
  , brackets
  , vars
  , name
  , varName
  , directiveName
  , enumVal
  , boolVal
  , intVal
  , doubleVal
  , stringVal
  )
  where

import Control.Applicative ((<|>), empty)
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, label, try, choice, optional, between, many, manyTill, some, oneOf, satisfy, takeP, chunkToTokens, notFollowedBy)
import Text.Megaparsec.Char (string, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (chr, digitToInt)
import Data.Foldable (foldl', foldr)
import Data.Scientific (toRealFloat)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))

type Parser = Parsec Text Text

upper, lower, alpha, ws, num :: String
upper = "_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "_acbcdefghijklmnopqrstuvwxyz"
alpha = upper <> lower
ws    = "\x0020\x0009"
num   = ['0'..'9']

upperChar, lowerChar, alphaChar, alphaNumChar :: Parser Char
upperChar    = satisfy (`elem` upper)
lowerChar    = satisfy (`elem` lower)
alphaChar    = satisfy (`elem` alpha)
alphaNumChar = satisfy (`elem` (alpha <> num))

-- https://spec.graphql.org/June2018/#sec-White-Space
whiteSpace :: Parser ()
whiteSpace
  = label "WhiteSpace" $ void $ satisfy (`elem` ws)
-- http://spec.graphql.org/June2018/#sec-Line-Terminators
lineTerminator :: Parser ()
lineTerminator
  = label "LineTerminator" $ void
  $ choice
    [ char '\x000A'
    , char '\x000D' <* optional (char '\x000A')
    ]
-- http://spec.graphql.org/June2018/#sec-Insignificant-Commas
insignificantComma :: Parser ()
insignificantComma
  = label "InsignificantComma" $ void $ char ','
-- http://spec.graphql.org/June2018/#sec-Unicode
unicodeBOM :: Parser ()
unicodeBOM
  = label "UnicodeBOM" $ void $ char '\xFEFF'
-- http://spec.graphql.org/June2018/#sec-Source-Text.Ignored-Tokens
ignoredTokens :: Parser ()
ignoredTokens
  = choice
    [ whiteSpace
    , lineTerminator
    , insignificantComma
    , unicodeBOM
    ]

-- http://spec.graphql.org/June2018/#sec-Comments
sc :: Parser ()
sc = L.space ignoredTokens (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

between_ :: Parser sep -> Parser a -> Parser a
between_ a = between a a

braces, parens, brackets :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

vars :: (forall a. Parser a -> Parser a) -> Parser k -> Parser v -> Parser [(k, v)]
vars f k v = label "Variables" $ f $ many $ (,) <$> (k <* symbol ":") <*> v

-- https://spec.graphql.org/June2018/#sec-Names
name :: Parser Text
name = label "Name" $ lexeme $ do
  a  <- alphaChar
  as <- many alphaNumChar
  pure $ Text.pack (a:as)

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
  [ True  <$ string "true"
  , False <$ string "false"
  ]

signed :: Num a => Parser a -> Parser a
signed = L.signed empty

-- http://spec.graphql.org/June2018/#sec-Int-Value
intVal :: Parser Int
intVal = label "IntVal" $ lexeme $ signed L.decimal <* notFollowedBy (symbol ".")

-- http://spec.graphql.org/June2018/#sec-Float-Value
doubleVal :: Parser Double
doubleVal = label "DoubleVal" $ lexeme $ choice
  [ toRealFloat <$> signed L.scientific
  , signed L.float
  ]

-- http://spec.graphql.org/June2018/#SourceCharacter
sourceChar
  = '\x0009'
  : '\x000A'
  : '\x000D'
  : ['\x0020' .. '\xFFFF']
  `except` "\""

except :: String -> String -> String
except = foldr List.delete

-- http://spec.graphql.org/June2018/#EscapedUnicode
-- http://spec.graphql.org/June2018/#EscapedCharacter
-- https://github.com/bens/caraus-graphql/blob/cbccb9ed0b32167dbb4de16eb5143dd62f9f3159/src/Language/GraphQL/AST/Lexer.hs#L204
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
        'u' -> chr . foldl' step 0
                   . chunkToTokens (Proxy @Text)
                 <$> takeP Nothing 4
        _ -> return escaped
  where
    step accumulator = (accumulator * 16 +) . digitToInt

sc' :: Parser ()
sc' = L.space lineTerminator empty empty

symbol' :: Text -> Parser Text
symbol' = L.symbol sc'

lineString :: Parser Text
lineString = label "InlineString" $ lexeme $ Text.pack <$> between_ (symbol' "\"") (many chars)
  where
    chars = choice
      [ escapeSequence
      , satisfy (`elem` (sourceChar `except` "\n"))
      ]

blockString :: Parser Text
blockString = label "BlockString" $ lexeme $ delim >> Text.pack <$> manyTill chars delim
  where
    delim = symbol' "\"\"\""
    chars = choice
      [ escapeSequence
      , satisfy (`elem` sourceChar)
      ]

stringVal :: Parser Text
stringVal = label "StringVal" $ choice [ blockString, lineString ] <* sc
