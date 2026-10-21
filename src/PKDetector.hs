
-- | PKDetector.hs

{-# LANGUAGE OverloadedStrings #-}

module PKDetector (parseQuery) where


import Language.SQL.SimpleSQL.Parse (ansi2011, parseStatement, prettyError)
import Language.SQL.SimpleSQL.Pretty (prettyStatement)
import Data.Either (fromRight, isLeft)
import Data.Maybe (fromJust, isJust, Maybe (Nothing))

import HintParser (HintFlag (..), parseHintLine, SQLHint (..))
import QueryParser (foldQuery, SQLQuery (..), statementJoinTargets)

import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as TextIO
import qualified Text.Parsec as TP
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import qualified DLLParser as DLL 
 

import qualified Data.ByteString as B
import Data.Text.Encoding as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL
--import Data.Aeson.Encode.Pretty
import Control.Monad
import Data.Either

data RawSQLPair
    = RawHQPair T.Text T.Text --Bool -- True = has hint
    deriving (Eq, Show)

data SQLPair 
    = HQPair SQLHint SQLQuery
    deriving (Eq, Show)

data FlaggedQuery 
    = Table T.Text
    | JoinClause FlaggedQuery [ T.Text ] FlaggedQuery [ T.Text ] HintFlag
    deriving (Eq, Show)

foldFlaggedQuery :: (T.Text -> a) 
                 -> (a -> [ T.Text ] -> a -> [ T.Text ] -> HintFlag -> a)
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
                  `T.append` (if T.null e2 then (T.pack "''") else e2)
                  `T.append` (T.pack " do not match. "))

hintQueryMatch :: SQLPair -> Maybe T.Text
hintQueryMatch hqp = case hqp of
    HQPair NoHint _ -> Nothing
    HQPair (Malformed _) (UnsupportedQuery _) -> Nothing
    HQPair (HintParser.Alias a) (QueryParser.Table _ alias) -> 
        hintQueryComparison a alias
    HQPair (HintParser.Name n) (QueryParser.Table name _)   -> 
        hintQueryComparison n name
    HQPair (Join h1 h2 _) (JoinOnClause q1 q2 _ _)  -> 
        (<>) (hintQueryMatch (HQPair h1 q1)) (hintQueryMatch (HQPair h2 q2))
    HQPair (Join h1 h2 _) (JoinUsingClause q1 q2 _) ->
        (<>) (hintQueryMatch (HQPair h1 q1)) (hintQueryMatch (HQPair h2 q2))
    _      -> Just (T.pack "Hint and query structures are not equivalent. ")

tablesPresent :: [ (T.Text, [ T.Text ]) ] -> SQLQuery -> Bool
tablesPresent l query = 
    List.foldr (\t rec -> t `List.elem` tables && rec) True (tableNames query)
  where
    tables     = Prelude.map fst l
    tableNames = foldQuery (\name  _   -> [name]) 
                           (\t1 t2 _ _ -> (++) t1 t2)
                           (\t1 t2 _   -> (++) t1 t2) 
                           (const [])

unknownTable :: [ (T.Text, [ T.Text ]) ] -> SQLQuery -> [ T.Text ]
unknownTable l query = 
    List.filter (\t -> not (t `List.elem` tables)) (tableNames query)
  where
    tables     = Prelude.map fst l
    tableNames = foldQuery (\name  _   -> [name]) 
                           (\t1 t2 _ _ -> (++) t1 t2)
                           (\t1 t2 _   -> (++) t1 t2) 
                           (const [])

safetyCheck :: [ (T.Text, [ T.Text ]) ] -> SQLPair -> Maybe T.Text
safetyCheck base hqp@(HQPair _ sqlQuery)
    | errorPresent hqp                  = Just (hintQueryError hqp)
    | isJust structureCheck             = structureCheck
    | not (tablesPresent base sqlQuery) = Just (T.pack $ "Non existent table " 
                                        ++ T.unpack (Prelude.head tableNotPresent))
    | otherwise = Nothing
  where
    structureCheck = hintQueryMatch hqp
    tableNotPresent = unknownTable base sqlQuery

queryTree :: SQLPair -> FlaggedQuery
queryTree (HQPair sqlh sqlq) = foldQuery 
    (\name  _           -> \_ -> PKDetector.Table name) 
    (\recQ1 recQ2 f1 f2 -> joinClause recQ1 recQ2 f1 f2)
    (\recQ1 recQ2 f     -> joinClause recQ1 recQ2 f f)
    (\_                 -> \_ -> PKDetector.Table T.empty) sqlq sqlh
  where
    joinClause r1 r2 f1 f2 q = case q of
        NoHint       -> JoinClause (r1 NoHint) f1 (r2 NoHint) f2 NoFlag
        Join h1 h2 h -> JoinClause (r1 h1) f1 (r2 h2) f2 h
        _            -> PKDetector.Table T.empty

pkFrom :: T.Text -> [ (T.Text, [ T.Text ]) ] -> [ T.Text ]
pkFrom e = List.foldr (\(t,pk) rec -> if t == e then pk else rec) [ ]

pkFromQuery :: [ (T.Text, [ T.Text ]) ] -> SQLPair -> Either T.Text (T.Text, [ T.Text ])
pkFromQuery base sqlp 
    | isJust checks = Left $ fromJust checks
    | otherwise     = either Left (\t -> Right (t, pkFrom t base)) $ 
        foldFlaggedQuery Right (\rq1 f1 rq2 f2 h -> 
            if isLeft rq1 || pkMatch rq2 f2
            then rq1
            else if pkMatch rq1 f1 then rq2 else flagJoin rq1 rq2 h) fq
  where
    checks             = safetyCheck base sqlp
    fq                 = queryTree sqlp
    pkMatch rq t       = t == pkFrom (fromRight T.empty rq) base
    hintFlag AllowNoPK = T.pack " WITH allow."
    hintFlag NoFlag    = T.pack " WITHOUT allow."
    flagJoin t1 t2 h   = Left $ T.pack "PK-less join between " 
                                `T.append` (either id id t1) 
                                `T.append` T.pack " and " 
                                `T.append` (either id id t2) 
                                `T.append` hintFlag h

stripVar :: [ T.Text ] -> [ T.Text ]
stripVar []                = []
stripVar (varLine : query) = T.tail (T.dropWhile (/= '`') varLine) : query

parsedHint :: T.Text -> SQLHint
parsedHint hint 
    | T.null hint = NoHint
    | otherwise = case TP.parse parseHintLine "" hint of
    Left  err    -> Malformed err
    Right parsed -> parsed

parsedQuery :: T.Text -> SQLQuery
parsedQuery query = either (UnsupportedQuery. prettyError) statementJoinTargets
                  $ parseStatement ansi2011 (T.pack "") Nothing query

parsePair :: RawSQLPair -> SQLPair
parsePair (RawHQPair h q) = HQPair (parsedHint h) (parsedQuery q)

rawCode :: [ T.Text ] -> [ RawSQLPair ]
rawCode []          = []
rawCode (line : xs)
    | T.pack hintPrefix `T.isPrefixOf` line = 
        RawHQPair line query : rawCode (snd $ queryAndRest xs)
    | T.pack queryPrefix `T.isPrefixOf` line = 
        RawHQPair T.empty queryOnly : rawCode (snd $ queryAndRest (line:xs))
    | otherwise = rawCode xs
  where
    hintPrefix     = "// sql-hint "
    queryPrefix    = "var sqlQ"
    queryAndRest l =
        (\(q,r) -> if Prelude.null r || Prelude.head r == T.pack "`;" 
                   then (q,r) 
                   else (q ++ [Prelude.head r], Prelude.tail r)) 
        $ List.span (\l -> T.strip l /= T.pack "`;") l
    queryStrip     = (T.takeWhile (/= '`')) . T.unwords . stripVar . (List.map T.strip)
    query          = queryStrip (fst $ queryAndRest xs)
    queryOnly      = queryStrip (fst $ queryAndRest (line:xs))

textFromEither :: Either T.Text (T.Text, [ T.Text ]) -> T.Text
textFromEither = 
    either id (\ts -> (T.pack "Final PK : ") `T.append` (T.intercalate ", " (pksText ts)))
  where
    pksText (t, l) = Prelude.map (pkText t) l
    pkText t pk = t `T.append` (T.pack ".") `T.append` pk

parseQuery :: FilePath -> FilePath -> IO ()
parseQuery dllFile sourceFile = do
    readDLL    <- Aeson.eitherDecode <$> BL.readFile dllFile
    readSource <- TextIO.readFile sourceFile

    let tablesAndPKs = DLL.parseDLLPks readDLL
    let pairs = rawCode $ T.lines readSource
    let processedPairs = Prelude.concatMap (\rawPair@(RawHQPair h q) -> 
          [ h 
          , (either prettyError (prettyStatement ansi2011) (queryStatement q))
          , T.empty
          , textFromEither . (pkFromQuery tablesAndPKs) $ parsePair rawPair
          , T.empty
          , T.replicate 30 (T.pack "-")]) pairs
    mapM_ Prelude.putStrLn $ Prelude.map T.unpack processedPairs
  where
    queryStatement q = (parseStatement ansi2011 (T.pack "") Nothing q)