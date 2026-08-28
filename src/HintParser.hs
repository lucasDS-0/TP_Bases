
-- | HintParser.hs

module HintParser where

import qualified Data.Text as T
import Text.Parsec.Text (Parser)
import Text.Parsec as TP
import Control.Monad


data Hint = AllowNoPK | NoFlag
  deriving (Eq, Show)

data SQLHint = Alias T.Text
             | Name T.Text
             | Join SQLHint SQLHint Hint
             | Malformed TP.ParseError
               deriving (Eq, Show)

parseHintLine :: Parser SQLHint
parseHintLine = do
  void $ string "// sql-hint "
  TP.spaces
  sqlHint <- parseStructure
  TP.spaces
  TP.eof
  return sqlHint

parseStructure :: Parser SQLHint
parseStructure = do
  sourceParser <|> joinParser

joinParser :: Parser SQLHint
joinParser = do
  void $ string "join"
  TP.char '('
  TP.spaces
  leftTarget <- parseStructure
  TP.spaces
  TP.char ','
  TP.spaces
  rightTarget <- parseStructure
  TP.spaces
  TP.char ','
  TP.spaces
  hint <- parseHint
  TP.spaces
  TP.char ')'
  return (Join leftTarget rightTarget hint)

parseHint :: Parser Hint
parseHint = do
  noPKParser <|> noFlagParser

noPKParser :: Parser Hint
noPKParser = do
  void $ string "allow-no-pk"
  return AllowNoPK

noFlagParser :: Parser Hint
noFlagParser = do
  void $ string "no-flag"
  return NoFlag

sourceParser :: Parser SQLHint
sourceParser = do
  aliasParser <|> nameParser

aliasParser :: Parser SQLHint
aliasParser = do
  void $ string "alias "
  alias <- TP.many1 TP.alphaNum
  return (Alias (T.pack alias))

nameParser :: Parser SQLHint
nameParser = do
  void $ string "name "
  name <- TP.many1 TP.alphaNum
  return (Name (T.pack name))