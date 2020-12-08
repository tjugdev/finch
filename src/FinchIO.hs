{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FinchIO
    ( FinchIO (getLine, getChar, print, random)
    , MockIO (mockStdout)
    , makeMockIO
    , makeMockIOWithRandom
    ) where

import Control.Monad.State
import System.Random (randomIO)

class Monad m => FinchIO m where
    getLine :: m String
    getChar :: m Char
    print :: String -> m ()
    random :: m Int

instance FinchIO IO where
    getLine = Prelude.getLine
    getChar = Prelude.getChar
    print = Prelude.putStr
    random = randomIO :: IO Int

data MockIO = MockIO
    { mockStdin        :: String
    , mockStdout       :: String
    , mockRandomStream :: [Int]
    } deriving (Show, Eq)

instance FinchIO (State MockIO) where
    getLine = state getLine'
      where
        formatResult mockIO (res, rest)
            | rest == [] = (res, mockIO { mockStdin = [] })
            | otherwise  = (res, mockIO { mockStdin = tail rest })
        getLine' mockIO@(MockIO stdin _ _)
            | stdin  == [] = ("", mockIO { mockStdin = [] })
            | otherwise    = formatResult mockIO $ break (== '\n') stdin

    getChar = state getChar'
      where
        getChar' mockIO@(MockIO stdin _ _)
            | stdin == [] = ('\0', mockIO { mockStdin = [] })
            | otherwise   = (head stdin, mockIO { mockStdin = tail stdin })

    print str = state print'
      where
        print' mockIO = ((), mockIO { mockStdout = (mockStdout mockIO ++ str) })

    random = state random'
      where
        random' mockIO@(MockIO _ _ randomStream)
            | randomStream == [] = (0, mockIO { mockRandomStream = [] })
            | otherwise =
                (head randomStream, mockIO { mockRandomStream = tail randomStream })

makeMockIOWithRandom :: String -> [Int] -> MockIO
makeMockIOWithRandom stdin randomStream = MockIO
    { mockStdin = stdin
    , mockStdout = ""
    , mockRandomStream = randomStream
    }

makeMockIO :: String -> MockIO
makeMockIO stdin = makeMockIOWithRandom stdin []
