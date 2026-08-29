
-- | DLLParser.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DLLParser where

import Data.Aeson
import Data.Text as T
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
    , uk_list :: [ T.Text ]
    , pk      :: [ T.Text ]
    } deriving (Show, Generic)

instance FromJSON Table

newtype Base = Base [ Table ] deriving (Show, Generic)

instance FromJSON Base

uniqueKey :: Table -> (T.Text, [ T.Text ])
uniqueKey t = (name t, uk_list t)

primaryKey :: Table -> (T.Text, [ T.Text ])
primaryKey t = (name t, pk t)

parseDLL :: Either String Base -> String
parseDLL f = case f of
    Left err            -> err
    Right (Base tablas) -> "[" 
      ++ (Data.List.intercalate "," (Prelude.map (showCorrectly . primaryKey) tablas)) 
      ++ "]"
  where
    showCorrectly (tab, pks) = "(" 
                            ++ (T.unpack tab) 
                            ++ ", " 
                            ++ (Data.List.intercalate ", " $ Prelude.map T.unpack pks) 
                            ++ ")"