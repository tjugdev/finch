module Main where

import Finch
import qualified Playfield
import Control.Exception (bracket)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))

runFile :: String -> IO ()
runFile file = bracket
    (openFile file ReadMode)
    hClose
    (\h -> do
        contents <- hGetContents h
        runString Playfield.standardWidth Playfield.standardHeight contents
        return ()
    )

main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then exitFailure else runFile $ head args
