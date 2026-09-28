
-- | DLLParser.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DLLParser (parseDLLPks) where

import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

data Field = Field 
    { field_name :: Text.Text
    , field_type :: Text.Text
    , not_null   :: Bool
    } deriving (Show, Generic) 

instance Aeson.FromJSON Field

data FKey = FKey
    { fk_name    :: Text.Text
    , references :: Text.Text
    } deriving (Show, Generic)

instance Aeson.FromJSON FKey

data Table = Table 
    { name    :: Text.Text
    , fields  :: [ Field ] 
    , fk_list :: [ FKey ]
    , pk      :: Text.Text
    } deriving (Show, Generic)

instance Aeson.FromJSON Table

newtype Base 
    = Base [ Table ] 
    deriving (Show, Generic)

instance Aeson.FromJSON Base

primaryKey :: Table -> (Text.Text, Text.Text)
primaryKey t = (name t, pk t)

parseDLLPks :: Either String Base -> [ (Text.Text, Text.Text) ]
parseDLLPks f = case f of
    Left err            -> [ (Text.pack err, Text.empty) ]
    Right (Base tablas) -> Prelude.map primaryKey tablas