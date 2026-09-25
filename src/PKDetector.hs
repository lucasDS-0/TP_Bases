
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
import Data.Either
import HintParser (Hint(AllowNoPK, NoFlag))


data SQLPair = HQPair SQLHint SQLQuery
  deriving (Eq, Show)

-- left-recursive 
data FlaggedQuery = Table T.Text
                  -- nombre_de_tabla pk_de_tabla
                  | JoinClause FlaggedQuery T.Text FlaggedQuery T.Text Hint
                  -- target_izq field_izq target_der field_der hint
                  deriving (Eq, Show)

foldFlaggedQuery :: (T.Text -> a) 
                 -> (a -> T.Text -> a -> T.Text -> Hint -> a)
                 -> FlaggedQuery
                 -> a
foldFlaggedQuery fTable fJoin fq = case fq of
    PKDetector.Table tt -> fTable tt
    JoinClause fq1 t1 fq2 t2 h -> fJoin (rec fq1) t1 (rec fq2) t2 h
  where
    rec = foldFlaggedQuery fTable fJoin

-- Pre: Las tablas utilizadas están presentes en la lista de tablas de la base
pkFrom :: T.Text -> [ (T.Text, T.Text) ] -> T.Text
pkFrom e = Data.List.foldr (\(t,pk) rec -> if t == e then pk else rec) T.empty

pkFromQuery :: [ (T.Text, T.Text) ] -> FlaggedQuery -> Either T.Text (T.Text, T.Text)
pkFromQuery b fq = either Left (\t -> Right (t, pkFrom t b)) $ 
  foldFlaggedQuery Right (\rq1 t1 rq2 t2 h -> 
    if isLeft rq1 || (rq2, t2) == (rq2, pkFrom (fromRight T.empty rq2) b)
    then rq1
    else if (rq1, t1) == (rq1, pkFrom (fromRight T.empty rq1) b)
         then rq2
         else flagJoin t1 t2 h) fq
  where
    flagJoin t1 t2 h = Left $ T.pack "Join de " `T.append` t1 `T.append` 
                              T.pack " y " `T.append` t2 `T.append` hintFlag h
    hintFlag AllowNoPK = T.pack " sin PK con allow."
    hintFlag NoFlag = T.pack " sin PK sin allow."

-- PRE: hint y query tienen la misma estructura
queryTree :: SQLPair -> FlaggedQuery
queryTree (HQPair sqlh sqlq) = 
  foldQuery (\name _ -> \_ -> PKDetector.Table name) 
            (\recQ1 recQ2 f1 f2 -> 
              \(Join h1 h2 h) -> JoinClause (recQ1 h1) f1 (recQ2 h2) f2 h)
            (\recQ1 recQ2 f -> 
              \(Join h1 h2 h) -> JoinClause (recQ1 h1) f (recQ2 h2) f h)
            (\_ -> \_ -> PKDetector.Table T.empty) sqlq sqlh

readCode :: [ T.Text ] -> [ SQLPair ]
readCode []          = []
readCode (line : xs) = if T.pack hintPrefix `T.isPrefixOf` line
                       then HQPair (parseReadHint line) parsedQuery : readCode rest
                       else readCode xs
  where
    hintPrefix     = "// sql-hint "
    hintLength     = Prelude.length hintPrefix
    (qLines, rest) = Data.List.span (\l -> T.strip l /= T.pack "`;") xs
    query          = T.unwords . stripVar $ Data.List.map T.strip qLines
    parsedQuery    = either (UnsupportedQuery. prettyError) statementJoinTargets
                   $ parseStatement ansi2011 (T.pack "") Nothing query

parseReadHint :: T.Text -> SQLHint
parseReadHint hint = case TP.parse parseHintLine "" hint of
  Left err     -> Malformed err
  Right parsed -> parsed

tablesPresent :: [ (T.Text, T.Text) ] -> SQLQuery -> Bool
tablesPresent l query = 
    Data.List.foldr (\e rec -> e `Data.List.elem` tables) True (tableNames query)
  where
    tables = Prelude.map fst l
    tableNames = foldQuery (const $ (:[])) (\t1 t2 _ _ -> (++) t1 t2)
                           (\t1 t2 _ -> (++) t1 t2) (const [])

hintQueryMatch :: SQLPair -> Maybe T.Text
hintQueryMatch hqp = case hqp of
  HQPair (Malformed _) (UnsupportedQuery _) -> Nothing
  HQPair (HintParser.Alias a) (QueryParser.Table _ alias) -> 
    hintQueryComparison a alias
  HQPair (HintParser.Name n) (QueryParser.Table name _)   -> 
    hintQueryComparison n name
  HQPair (Join h1 h2 _) (JoinOnClause q1 q2 _ _)  -> 
    (<>) (hintQueryMatch (HQPair h1 q1)) (hintQueryMatch (HQPair h1 q2))
  HQPair (Join h1 h2 _) (JoinUsingClause q1 q2 _) ->
    (<>) (hintQueryMatch (HQPair h1 q1)) (hintQueryMatch (HQPair h2 q2))
  _      -> Just (T.pack "Hint and query structures are not equivalent.\n")

hintQueryComparison :: T.Text -> T.Text -> Maybe T.Text
hintQueryComparison e1 e2 = 
  if e1 == e2 
  then Nothing
  else Just (e1 `T.append` (T.pack " and ") 
                `T.append` e2 
                `T.append` (T.pack " do not match.\n"))

{--
calculatePK :: Base -> SQLPair -> Either T.Text T.Text
calculatePK base@(Base tablas) hqp = 
  if errorPresent hqp
  then Left (hintQueryError hqp)
  else  if isJust structureCheck
        then structureCheck
        else if not (tablesPresent tablas sqlQuery)
             then Left "Query involves non existent tables."
             else case sqlQuery of
                  Table name alias -> Right (pkFrom base name)
                  JoinOnClause q1 q2 lf rf -> pkCheck 
                  JoinUsingClause q1 q2 f  -> Right
                  _ -> Right T.empty
  where
    hqp = HQPair sqlHint sqlQuery
    structureCheck = hintQueryMatch hqp
--}

{--
pkCheck :: (T.Text, T.Text) -> (T.Text, T.Text) -> [(T.Text, T.Text)] 
        -> Either T.Text [(T.Text, T.Text)]
pkCheck l@(t1,_) r@(t2,_) pks = 
  if Data.List.elem l pks
  then Right (r:Prelude.filter (/= l) pks)
  else if Data.List.elem r pks
       then Right (l:Prelude.filter (/= r) pks)
       else if checkAllowNoPk 
            then Right (l:r:pks)
            else Left (T.pack ("Join sin allow-no-pk entre ") `append` 
                       t1 `append` (T.pack " y ") `append` t2)
--}
hintQueryError :: SQLPair -> T.Text
hintQueryError (HQPair (Malformed err1) (UnsupportedQuery err2)) = 
  T.append (T.pack $ Prelude.show err1) err2
hintQueryError (HQPair (Malformed err1) _)        = T.pack $ Prelude.show err1
hintQueryError (HQPair _ (UnsupportedQuery err2)) = err2
hintQueryError (HQPair _ _) = T.empty

errorPresent :: SQLPair -> Bool
errorPresent hqp = case hqp of
  HQPair (Malformed _) _        -> True
  HQPair _ (UnsupportedQuery _) -> True
  _                             -> False

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
    --Prelude.putStrLn $ Prelude.map (\(t,pk) -> (T.unpack t, T.unpack pk)) $ parseDLLPks readDLL
    let tablasYPKs = parseDLLPks readDLL
    mapM_ print $ tablasYPKs
    --mapM_ print $ Prelude.map (queryTree (fromRight (Base []) readDLL)) (readCode $ T.lines readHint)
    mapM_ print $ Prelude.map (pkFromQuery tablasYPKs)$ Prelude.map queryTree (readCode $ T.lines readHint)

