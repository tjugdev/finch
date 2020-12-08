{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module FinchIO
    ( FinchIO (getLine, getChar, print)
    , makeMockStdio
    , MockStdio (stdout)
    ) where

import Control.Monad.State

class Monad m => FinchIO m where
    getLine :: m String
    getChar :: m Char
    print :: String -> m ()

instance FinchIO IO where
    getLine = Prelude.getLine
    getChar = Prelude.getChar
    print = Prelude.putStr

data MockStdio = MockStdio { stdinChars :: String
                           , stdinStrings :: [String]
                           , stdout :: String
                           } deriving (Show, Eq)

instance FinchIO (State MockStdio) where
  getLine = state getLine'
    where
      getLine' mockStdio@(MockStdio _ (nextLine:lines) _) =
          (nextLine, mockStdio { stdinStrings = lines })
      getLine' mockStdio@(MockStdio _ [] _) = ("", mockStdio)
  getChar = state getChar'
    where
      getChar' mockStdio@(MockStdio (nextChar:chars) _ _) =
          (nextChar, mockStdio { stdinChars = chars })
      getChar' mockStdio@(MockStdio [] _ _) = ('\0', mockStdio)
  print str = state $ \mockStdio@(MockStdio _ _ prevStdout) ->
      ((), mockStdio { stdout = prevStdout ++ str })


makeMockStdio :: String -> [String] -> MockStdio
makeMockStdio inChars inStrings = MockStdio
    { stdinChars = inChars
    , stdinStrings = inStrings
    , stdout = ""
    }
