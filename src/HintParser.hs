
-- | HintParser.hs

module HintParser 
    ( foldHint
    , Hint (..)
    , SQLHint (..)
    , parseHintLine
    ) where

import Control.Monad (void)
import Text.Parsec.Text (Parser)
import Text.Parsec as TP ((<|>), alphaNum, char, eof, many1, ParseError, spaces, string)

import qualified Data.Text as Text
 

data Hint 
    = AllowNoPK 
    | NoFlag 
    deriving (Eq, Show)

data SQLHint 
    = Alias Text.Text
    | Name Text.Text
    | Join SQLHint SQLHint Hint
    | Malformed TP.ParseError
    deriving (Eq, Show)

foldHint :: (Text.Text -> a) 
         -> (Text.Text -> a) 
         -> (a -> a -> Hint -> a) 
         -> (TP.ParseError -> a) 
         -> SQLHint
         -> a
foldHint fAlias fName fJoin fMalformed sqlh = case sqlh of
    Alias alias        -> fAlias alias
    Name name          -> fName name
    Join sqlh1 sqlh2 h -> fJoin (hrec sqlh1) (hrec sqlh2) h
    Malformed err      -> fMalformed err
  where
    hrec = foldHint fAlias fName fJoin fMalformed

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
  _ <- TP.char '('
  TP.spaces
  leftTarget <- parseStructure
  TP.spaces
  _ <- TP.char ','
  TP.spaces
  rightTarget <- parseStructure
  TP.spaces
  _ <- TP.char ','
  TP.spaces
  hint <- parseHint
  TP.spaces
  _ <- TP.char ')'
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
  return (Alias (Text.pack alias))

nameParser :: Parser SQLHint
nameParser = do
  void $ string "name "
  name <- TP.many1 TP.alphaNum
  return (Name (Text.pack name))