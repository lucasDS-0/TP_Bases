
-- | Main.hs

module Main where

import PKDetector
import GHC.Internal.System.Environment (getArgs)

defaultDLLFile :: FilePath
defaultDLLFile = "ddl.json"

defaultQueryFile :: FilePath
defaultQueryFile = "sql_hint.ts"

main :: IO ()
main = do
    args <- getArgs
    parseQuery (head args) (args!!1)