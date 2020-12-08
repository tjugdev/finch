module Main (main) where

import Control.Exception (bracket)
import Finch
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

runFile :: String -> IO ()
runFile file = bracket
    (openFile file ReadMode)
    hClose
    (\h -> hGetContents h >>= runStringStandardSize_)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then exitFailure else runFile $ head args
