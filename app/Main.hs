
-- | Main.hs

module Main (main) where

import Data.List (isSuffixOf)
import GHC.Internal.System.Environment (getArgs)

import PKDetector 

main :: IO ()
main = do
    args <- getArgs
    case args of
        (dll:query:_) -> if ".json" `isSuffixOf` dll && ".ts" `isSuffixOf` query
                         then parseQuery dll query
                         else putStrLn "Proveer primer un archivo .json y luego uno .ts."
        _             -> putStrLn "Cantidad erronea de argumentos."