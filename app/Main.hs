module Main (main) where


import DLLParser
import PKDetector
import GHC.Internal.System.Environment (getArgs)

defaultDLLFile :: FilePath
defaultDLLFile = "ddl.json"

defaultQueryFile :: FilePath
defaultQueryFile = "sql_hint.ts"

main :: IO ()
main = do
    args <- getArgs
    parseDLL (head args)
    parseQuery (args!!1)