
-- | PKDetector.hs

{-# LANGUAGE OverloadedStrings #-}

module PKDetector (parseQuery) where

import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Syntax as S
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Pretty
import qualified Language.SQL.SimpleSQL.Lex as L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.IO as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL
import Data.Aeson.Encode.Pretty
import Data.List
import GHC.Base (undefined)
import Text.Parsec as TP
import Text.Parsec.Text (Parser)
import Control.Monad


queryFile :: FilePath
queryFile = "query_general.sql"

data Hint = AllowNoPK | NoFlag
  deriving (Eq, Show)

data SQLHint = Alias T.Text
             | Name T.Text
             | Join SQLHint SQLHint Hint
             | Malformed TP.ParseError
               deriving (Eq, Show)

data SQLPair = HQPair SQLHint SQLQuery
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
  return (PKDetector.Alias (T.pack alias))

nameParser :: Parser SQLHint
nameParser = do
  void $ string "name "
  name <- TP.many1 TP.alphaNum
  return (PKDetector.Name (T.pack name))

readCode :: [ T.Text ] -> [ SQLPair ]
readCode []          = []
readCode (line : xs) = if T.pack hintPrefix `T.isPrefixOf` line
                       then HQPair (readParseHint line) parsedQuery : readCode rest
                       else readCode xs
  where
    hintPrefix     = "// sql-hint "
    hintLength     = Prelude.length hintPrefix
    (qLines, rest) = Data.List.span (\l -> T.strip l /= T.pack "`;") xs
    query          = T.unwords . stripVar $ Data.List.map T.strip qLines
    parsedQuery    = either (UnsupportedQuery. prettyError) statementJoinTargets
                   $ parseStatement ansi2011 (T.pack "") Nothing query

readParseHint :: T.Text -> SQLHint
readParseHint hint = case TP.parse parseHintLine "" hint of
  Left err     -> Malformed err
  Right parsed -> parsed

stripVar :: [ T.Text ] -> [ T.Text ]
stripVar [] = []
stripVar (varLine : query) = T.tail (T.dropWhile (/= '`') varLine) : query

getQuery :: IO BL.ByteString
getQuery = BL.readFile queryFile

nameAsText :: Name -> T.Text
nameAsText (S.Name _ name) = name

namesAsText :: [ Name ] -> [ T.Text ]
namesAsText = Prelude.map nameAsText

data Method = On | Using
  deriving (Eq, Show)

data SQLQuery = Table T.Text T.Text
              -- name alias
              | JoinOnClause SQLQuery SQLQuery T.Text T.Text
              -- left_target right_target left_field right_field
              | JoinUsingClause SQLQuery SQLQuery T.Text
              -- left_target right_target field
              | UnsupportedQuery T.Text
              -- query not supported
                deriving (Eq, Show)

extractAlias :: Alias -> T.Text
extractAlias (S.Alias name _) = nameAsText name

extractTargets :: TableRef -> SQLQuery
extractTargets (TRSimple [name]) = PKDetector.Table (nameAsText name) (nameAsText name)
extractTargets (TRQueryExpr Select {qeFrom=[tr]}) = extractTargets tr
extractTargets (TRAlias (TRSimple [name]) alias) = 
  PKDetector.Table (nameAsText name) (extractAlias alias)
extractTargets (TRAlias (TRQueryExpr Select {qeFrom=[tr]}) alias) = 
  PKDetector.Table (((\(PKDetector.Table name _) -> name) . extractTargets) tr) 
                   (extractAlias alias)
extractTargets (TRJoin tra _ joinType trb (Just (JoinUsing [name]))) = 
  if joinType `notElem` [JInner, JLeft]
  then UnsupportedQuery (T.pack "")
  else JoinUsingClause (extractTargets tra) (extractTargets trb) (nameAsText name)
extractTargets (TRJoin tra _ joinType trb (Just (JoinOn se))) = 
  if joinType `notElem` [JInner, JLeft]
  then UnsupportedQuery (T.pack "")
  else uncurry (JoinOnClause (extractTargets tra) (extractTargets trb)) (joinOnTargets se)
extractTargets _ = UnsupportedQuery (T.pack "")

joinOnTargets :: ScalarExpr -> (T.Text, T.Text)
joinOnTargets (BinOp (Iden e1) _ (Iden e2)) = (namesAsText e1!!1, namesAsText e2!!1)
joinOnTargets _                             = (T.pack "", T.pack "")

{--
data Operation = InnerJoin | LeftJoin
  deriving (Eq, Show)

data Method = On | Using
  deriving (Eq, Show)

--}

{--
statementJoinTargets :: Statement -> [ (T.Text, T.Text) ]
statementJoinTargets (SelectStatement (Select{qeFrom= [fromRecord]})) =
    embelishTargets $ extractTargets fromRecord
--}

statementJoinTargets :: Statement -> SQLQuery
statementJoinTargets (SelectStatement (Select{qeFrom= [fromRecord]})) =
    extractTargets fromRecord

parseQuery :: FilePath -> IO ()
parseQuery file = do
    -- archivos
    --query <- getQuery
    hintFile <- T.readFile file

    -- query completa
    --Prelude.putStrLn $ either (T.unpack . prettyError) (T.unpack . prettyStatement ansi2011) $ parseStatement ansi2011 (T.pack "") Nothing (T.decodeUtf8 (B.concat $ BL.toChunks query))

    --PKs de tablas
    --Prelude.putStrLn $ either (T.unpack . prettyError) (show . statementJoinTargets)
      --            $ parseStatement ansi2011 (T.pack "") Nothing (T.decodeUtf8 (B.concat $ BL.toChunks query))

    --Parseado
    --Prelude.putStrLn $ either (T.unpack . prettyError) (TL.unpack . TL.decodeUtf8 . encodePretty . show)
      --        $ parseStatement ansi2011 (T.pack "") Nothing (T.decodeUtf8 (B.concat $ BL.toChunks query))

    -- HQPairs
    mapM_ print $ readCode $ T.lines hintFile
