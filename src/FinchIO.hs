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
    getLine = state $ \mockIO@(MockIO stdin _ _) ->
        (\x -> mockIO { mockStdin = tailSafe x }) <$> break (== '\n') stdin

    getChar = state $ \mockIO@(MockIO stdin _ _) ->
        (headDef '\0' stdin, mockIO { mockStdin = tailSafe stdin })

    print str = state $ \mockIO ->
        ((), mockIO { mockStdout = (mockStdout mockIO ++ str) })

    random = state $ \mockIO@(MockIO _ _ randomStream) ->
        (headDef 0 randomStream, mockIO { mockRandomStream = tailSafe randomStream })

makeMockIO :: String -> [Int] -> MockIO
makeMockIO stdin randomStream = MockIO
    { mockStdin = stdin
    , mockStdout = ""
    , mockRandomStream = randomStream
    }
