
-- | Main.hs

module Main (main) where

import PKDetector 
import GHC.Internal.System.Environment (getArgs)

defaultDLLFile :: FilePath
defaultDLLFile = "ddl.json"

defaultQueryFile :: FilePath
defaultQueryFile = "sql_hint.ts"

main :: IO ()
main = do
    args <- getArgs
    case args of
        (dll:query:_) -> parseQuery dll query
        _             -> parseQuery defaultDLLFile defaultQueryFile