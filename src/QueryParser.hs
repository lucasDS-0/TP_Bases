
-- | QueryParser.hs

module QueryParser 
    ( statementJoinTargets
    , foldQuery
    , SQLQuery (..)
    ) where


import Language.SQL.SimpleSQL.Syntax as S

import qualified Data.Text as Text


nameAsText :: Name -> Text.Text
nameAsText (S.Name _ name) = name

namesAsText :: [ Name ] -> [ Text.Text ]
namesAsText = Prelude.map nameAsText

data SQLQuery 
    = Table Text.Text Text.Text
    | JoinOnClause SQLQuery SQLQuery Text.Text Text.Text
    | JoinUsingClause SQLQuery SQLQuery Text.Text
    | UnsupportedQuery Text.Text
    deriving (Eq, Show)

foldQuery :: (Text.Text -> Text.Text -> a)
          -> (a -> a -> Text.Text -> Text.Text -> a)
          -> (a -> a -> Text.Text -> a)
          -> (Text.Text -> a)
          -> SQLQuery
          -> a
foldQuery fTable fJoinOn fJoinUsing fUnsupported q = case q of
    QueryParser.Table name alias -> fTable name alias
    JoinOnClause q1 q2 lf rf     -> fJoinOn (qrec q1) (qrec q2) lf rf
    JoinUsingClause q1 q2 f      -> fJoinUsing (qrec q1) (qrec q2) f
    UnsupportedQuery err         -> fUnsupported err
  where
    qrec = foldQuery fTable fJoinOn fJoinUsing fUnsupported

extractAlias :: Alias -> Text.Text
extractAlias (Alias name _) = nameAsText name

extractTargets :: TableRef -> SQLQuery
extractTargets t = case t of
    TRSimple [name] -> QueryParser.Table (nameAsText name) (Text.empty)
    TRQueryExpr Select {qeFrom=[tr]} -> extractTargets tr
    TRAlias (TRSimple [name]) alias ->
        QueryParser.Table (nameAsText name) (extractAlias alias)
    TRAlias (TRQueryExpr Select {qeFrom=[tr]}) alias ->
        subQueryTable tr alias
    TRJoin tra _ joinType trb (Just cond) ->
        if joinType `notElem` [JInner, JLeft]
        then UnsupportedQuery (Text.pack "Invalid Join.")
        else case cond of
            JoinUsing [name] ->
              JoinUsingClause (extractTargets tra) (extractTargets trb) (nameAsText name)
            JoinOn se        ->
              uncurry (JoinOnClause (extractTargets tra) (extractTargets trb)) 
                  (joinOnTargets se)
            _                -> UnsupportedQuery (Text.pack "Invalid Join.")
    _  -> UnsupportedQuery (Text.pack "Invalid Query.")
  where
    subQueryTable tr alias = case extractTargets tr of
        QueryParser.Table name _ -> QueryParser.Table name (extractAlias alias)
        _                        -> UnsupportedQuery (Text.pack "Invalid Query.")

joinOnTargets :: ScalarExpr -> (Text.Text, Text.Text)
joinOnTargets (BinOp (Iden e1) _ (Iden e2)) = (namesAsText e1!!1, namesAsText e2!!1)
joinOnTargets _                             = (Text.pack "", Text.pack "")

statementJoinTargets :: Statement -> SQLQuery
statementJoinTargets (SelectStatement (Select{qeFrom= [fromRecord]})) =
    extractTargets fromRecord
statementJoinTargets _ = UnsupportedQuery (Text.pack "Invalid Query.")