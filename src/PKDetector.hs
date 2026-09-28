
-- | PKDetector.hs

{-# LANGUAGE OverloadedStrings #-}

module PKDetector (parseQuery) where


import Language.SQL.SimpleSQL.Parse (ansi2011, parseStatement, prettyError)
import Data.Either (fromRight, isLeft)
import Data.Maybe (fromJust, isJust)

import HintParser (Hint (..), parseHintLine, SQLHint (..))
import QueryParser (foldQuery, SQLQuery (..), statementJoinTargets)

import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as TextIO
import qualified Text.Parsec as TP
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import qualified DLLParser as DLL
 

data SQLPair 
    = HQPair SQLHint SQLQuery
    deriving (Eq, Show)

data FlaggedQuery 
    = Table T.Text
    | JoinClause FlaggedQuery T.Text FlaggedQuery T.Text Hint
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

errorPresent :: SQLPair -> Bool
errorPresent hqp = case hqp of
    HQPair (Malformed _) _        -> True
    HQPair _ (UnsupportedQuery _) -> True
    _                             -> False

hintQueryError :: SQLPair -> T.Text
hintQueryError (HQPair (Malformed err1) (UnsupportedQuery err2)) = 
    T.append (T.pack $ Prelude.show err1) err2
hintQueryError (HQPair (Malformed err1) _)        = T.pack $ Prelude.show err1
hintQueryError (HQPair _ (UnsupportedQuery err2)) = err2
hintQueryError (HQPair _ _) = T.empty

hintQueryComparison :: T.Text -> T.Text -> Maybe T.Text
hintQueryComparison e1 e2
    | e1 == e2  = Nothing
    | otherwise = Just (e1 `T.append` (T.pack " and ") 
                  `T.append` e2 
                  `T.append` (T.pack " do not match."))

hintQueryMatch :: SQLPair -> Maybe T.Text
hintQueryMatch hqp = case hqp of
    HQPair (Malformed _) (UnsupportedQuery _) -> Nothing
    HQPair (HintParser.Alias a) (QueryParser.Table _ alias) -> 
        hintQueryComparison a alias
    HQPair (HintParser.Name n) (QueryParser.Table name _)   -> 
        hintQueryComparison n name
    HQPair (Join h1 h2 _) (JoinOnClause q1 q2 _ _)  -> 
        (<>) (hintQueryMatch (HQPair h1 q1)) (hintQueryMatch (HQPair h2 q2))
    HQPair (Join h1 h2 _) (JoinUsingClause q1 q2 _) ->
        (<>) (hintQueryMatch (HQPair h1 q1)) (hintQueryMatch (HQPair h2 q2))
    _      -> Just (T.pack "Hint and query structures are not equivalent.")

tablesPresent :: [ (T.Text, T.Text) ] -> SQLQuery -> Bool
tablesPresent l query = 
    List.foldr (\t _ -> t `List.elem` tables) True (tableNames query)
  where
    tables     = Prelude.map fst l
    tableNames = foldQuery (\name _ -> [name]) (\t1 t2 _ _ -> (++) t1 t2)
                           (\t1 t2 _ -> (++) t1 t2) (const [])

safetyCheck :: [ (T.Text, T.Text) ] -> SQLPair -> Maybe T.Text
safetyCheck ts hqp@(HQPair _ sqlQuery)
    | errorPresent hqp                = Just (hintQueryError hqp)
    | isJust structureCheck           = structureCheck
    | not (tablesPresent ts sqlQuery) = Just (T.pack "Query uses non existent tables.")
    | otherwise = Nothing
  where
    structureCheck = hintQueryMatch hqp

pkFrom :: T.Text -> [ (T.Text, T.Text) ] -> T.Text
pkFrom e = List.foldr (\(t,pk) rec -> if t == e then pk else rec) T.empty

queryTree :: SQLPair -> FlaggedQuery
queryTree (HQPair sqlh sqlq) = foldQuery 
    (\name  _           -> \_ -> PKDetector.Table name) 
    (\recQ1 recQ2 f1 f2 -> joinClause recQ1 recQ2 f1 f2)
    (\recQ1 recQ2 f     -> joinClause recQ1 recQ2 f f)
    (\_                 -> \_ -> PKDetector.Table T.empty) sqlq sqlh
  where
    joinClause r1 r2 f1 f2 q = case q of
        Join h1 h2 h -> JoinClause (r1 h1) f1 (r2 h2) f2 h
        _            -> PKDetector.Table T.empty

pkFromQuery :: [ (T.Text, T.Text) ] -> SQLPair -> Either T.Text (T.Text, T.Text)
pkFromQuery b sqlp 
    | isJust checks = Left $ fromJust checks
    | otherwise     = either Left (\t -> Right (t, pkFrom t b)) $ 
        foldFlaggedQuery Right (\rq1 t1 rq2 t2 h -> 
            if isLeft rq1 || pkMatch rq2 t2
            then rq1
            else if pkMatch rq1 t1 then rq2 else flagJoin t1 t2 h) fq
  where
    checks             = safetyCheck b sqlp
    fq                 = queryTree sqlp
    pkMatch rq t       = t == pkFrom (fromRight T.empty rq) b
    hintFlag AllowNoPK = T.pack " with allow."
    hintFlag NoFlag    = T.pack " without allow."
    flagJoin t1 t2 h   = Left $ T.pack "PK-less join between " `T.append` t1 `T.append` 
                                T.pack " and " `T.append` t2 `T.append` hintFlag h

stripVar :: [ T.Text ] -> [ T.Text ]
stripVar []                = []
stripVar (varLine : query) = T.tail (T.dropWhile (/= '`') varLine) : query

parsedHint :: T.Text -> SQLHint
parsedHint hint = case TP.parse parseHintLine "" hint of
    Left  err    -> Malformed err
    Right parsed -> parsed

parsedQuery :: T.Text -> SQLQuery
parsedQuery query = either (UnsupportedQuery. prettyError) statementJoinTargets
                  $ parseStatement ansi2011 (T.pack "") Nothing query

readCode :: [ T.Text ] -> [ SQLPair ]
readCode []          = []
readCode (line : xs)
    | T.pack hintPrefix `T.isPrefixOf` line = 
        HQPair (parsedHint line) (parsedQuery query) : readCode rest
    | otherwise = readCode xs
  where
    hintPrefix     = "// sql-hint "
    (qLines, rest) = List.span (\l -> T.strip l /= T.pack "`;") xs
    query          = T.unwords . stripVar $ List.map T.strip qLines

parseQuery :: FilePath -> FilePath -> IO ()
parseQuery dllFile hintFile = do
    readDLL  <- Aeson.eitherDecode <$> BL.readFile dllFile
    readHint <- TextIO.readFile hintFile

    let tablasYPKs = DLL.parseDLLPks readDLL
    mapM_ print $ Prelude.map (pkFromQuery tablasYPKs) (readCode $ T.lines readHint)

