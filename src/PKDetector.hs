
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
import Text.Parsec as TP
import Text.Parsec.Text (Parser)
import Control.Monad
import HintParser
import QueryParser
import DLLParser
import Data.Aeson


queryFile :: FilePath
queryFile = "sql_hint.ts"

data SQLPair = HQPair SQLHint SQLQuery
  deriving (Eq, Show)

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

-- asdasdsad

parseQuery :: FilePath -> FilePath -> IO ()
parseQuery dllFile hintFile = do
    -- archivos
    --query <- getQuery
    readDLL  <- eitherDecode <$> BL.readFile dllFile
    readHint <- T.readFile hintFile

    -- query completa
    --Prelude.putStrLn $ either (T.unpack . prettyError) (T.unpack . prettyStatement ansi2011) $ parseStatement ansi2011 (T.pack "") Nothing (T.decodeUtf8 (B.concat $ BL.toChunks query))

    --PKs de tablas
    --Prelude.putStrLn $ either (T.unpack . prettyError) (show . statementJoinTargets)
      --            $ parseStatement ansi2011 (T.pack "") Nothing (T.decodeUtf8 (B.concat $ BL.toChunks query))

    --Parseado
    --Prelude.putStrLn $ either (T.unpack . prettyError) (TL.unpack . TL.decodeUtf8 . encodePretty . show)
      --        $ parseStatement ansi2011 (T.pack "") Nothing (T.decodeUtf8 (B.concat $ BL.toChunks query))

    -- HQPairs
    Prelude.putStrLn $ parseDLL readDLL
    mapM_ print $ readCode $ T.lines readHint
