
-- | DLLParser.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DLLParser where

import Data.Aeson
import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)

data Field = Field 
    { field_name :: T.Text
    , field_type :: T.Text
    , not_null   :: Bool
    } deriving (Show, Generic)

instance FromJSON Field

data FKey = FKey
    { fk_name    :: T.Text
    , references :: T.Text
    } deriving (Show, Generic)

instance FromJSON FKey

data Table = Table 
    { name    :: T.Text
    , fields  :: [ Field ] 
    , fk_list :: [ FKey ]
    , uk      :: T.Text
    , pk      :: T.Text
    } deriving (Show, Generic)

instance FromJSON Table

newtype Base = Base [ Table ] deriving (Show, Generic)

instance FromJSON Base

uniqueKey :: Table -> (T.Text, T.Text)
uniqueKey t = (name t, uk t)

primaryKey :: Table -> (T.Text, T.Text)
primaryKey t = (name t, pk t)

parseDLLPks :: Either String Base -> [ (T.Text, T.Text) ]
parseDLLPks f = case f of
    Left err            -> [ (T.pack err, T.empty) ]
    Right (Base tablas) -> Prelude.map primaryKey tablas 
      
showDLL :: Either String Base -> String
showDLL f = case f of
    Left err            -> err
    Right (Base tablas) -> "[" 
      ++ (intercalate "," (Prelude.map (showCorrectly . primaryKey) tablas)) 
      ++ "]"
  where
    showCorrectly (tab, pk)  = "(" ++ (T.unpack tab) ++ ", " ++ (T.unpack pk) ++ ")"