module Main (main) where

import Finch
import Control.Exception (bracket)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))

runFile :: String -> IO ()
runFile file = bracket
    (openFile file ReadMode)
    hClose
    (\h -> hGetContents h >>= runStringStandardSize_)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then exitFailure else runFile $ head args
