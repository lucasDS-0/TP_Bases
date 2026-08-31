
-- | QueryParser.hs

module QueryParser where

import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Syntax as S
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Pretty
import qualified Language.SQL.SimpleSQL.Lex as L
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.IO as T


stripVar :: [ T.Text ] -> [ T.Text ]
stripVar []                = []
stripVar (varLine : query) = T.tail (T.dropWhile (/= '`') varLine) : query

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
              -- error
                deriving (Eq, Show)

foldQuery :: (T.Text -> T.Text -> a)
          -> (a -> a -> T.Text -> T.Text -> a)
          -> (a -> a -> T.Text -> a)
          -> (T.Text -> a)
          -> SQLQuery
          -> a
foldQuery fTable fJoinOn fJoinUsing fUnsupported q = case q of
    QueryParser.Table name alias         -> fTable name alias
    JoinOnClause q1 q2 lf rf -> fJoinOn (qrec q1) (qrec q2) lf rf
    JoinUsingClause q1 q2 f  -> fJoinUsing (qrec q1) (qrec q2) f
    UnsupportedQuery err     -> fUnsupported err
  where
    qrec = foldQuery fTable fJoinOn fJoinUsing fUnsupported

extractAlias :: Alias -> T.Text
extractAlias (Alias name _) = nameAsText name

extractTargets :: TableRef -> SQLQuery
extractTargets t = case t of
    TRSimple [name] -> QueryParser.Table (nameAsText name) (nameAsText name)
    TRQueryExpr Select {qeFrom=[tr]} -> extractTargets tr
    TRAlias (TRSimple [name]) alias ->
      QueryParser.Table (nameAsText name) (extractAlias alias)
    TRAlias (TRQueryExpr Select {qeFrom=[tr]}) alias ->
      QueryParser.Table (((\(QueryParser.Table name _) -> name) . extractTargets) tr) 
                       (extractAlias alias)
    TRJoin tra _ joinType trb (Just cond) ->
      if joinType `notElem` [JInner, JLeft]
      then UnsupportedQuery (T.pack "")
      else case cond of
        JoinUsing [name] ->
          JoinUsingClause (extractTargets tra) (extractTargets trb) (nameAsText name)
        JoinOn se        ->
          uncurry (JoinOnClause (extractTargets tra) (extractTargets trb)) 
                  (joinOnTargets se)
    _  -> UnsupportedQuery (T.pack "")

joinOnTargets :: ScalarExpr -> (T.Text, T.Text)
joinOnTargets (BinOp (Iden e1) _ (Iden e2)) = (namesAsText e1!!1, namesAsText e2!!1)
joinOnTargets _                             = (T.pack "", T.pack "")

statementJoinTargets :: Statement -> SQLQuery
statementJoinTargets (SelectStatement (Select{qeFrom= [fromRecord]})) =
    extractTargets fromRecord