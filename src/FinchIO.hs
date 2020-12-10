{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FinchIO
    ( FinchIO (getLine, getChar, print, random)
    , MockIO (mockStdout)
    , makeMockIO
    ) where

import Control.Monad.State
import Safe (headDef, tailSafe)
import System.IO (hFlush, stdout)
import System.Random (randomIO)

class Monad m => FinchIO m where
    getLine :: m String
    getChar :: m Char
    print :: String -> m ()
    random :: m Int

instance FinchIO IO where
    getLine = hFlush stdout >> Prelude.getLine
    getChar = hFlush stdout >> Prelude.getChar
    print = Prelude.putStr
    random = randomIO

data MockIO = MockIO
    { mockStdin        :: String
    , mockStdout       :: String
    , mockRandomStream :: [Int]
    } deriving (Show, Eq)

instance FinchIO (State MockIO) where
    getLine = state getLine'
      where
        formatResult mockIO (res, rest) = (res, mockIO { mockStdin = tailSafe rest })
        getLine' mockIO@(MockIO stdin _ _) = formatResult mockIO $ break (== '\n') stdin

    getChar = state getChar'
      where
        getChar' mockIO@(MockIO stdin _ _) =
            (headDef '\0' stdin, mockIO { mockStdin = tailSafe stdin })

    print str = state print'
      where
        print' mockIO = ((), mockIO { mockStdout = (mockStdout mockIO ++ str) })

    random = state random'
      where
        random' mockIO@(MockIO _ _ randomStream) =
            (headDef 0 randomStream, mockIO { mockRandomStream = tailSafe randomStream })

makeMockIO :: String -> [Int] -> MockIO
makeMockIO stdin randomStream = MockIO
    { mockStdin = stdin
    , mockStdout = ""
    , mockRandomStream = randomStream
    }
