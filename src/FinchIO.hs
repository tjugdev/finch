{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FinchIO
    ( FinchIO (getLine, getChar, print)
    , makeMockStdio
    , MockStdio (mockStdout)
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

data MockStdio = MockStdio
    { mockStdin  :: String
    , mockStdout :: String
    } deriving (Show, Eq)

instance FinchIO (State MockStdio) where
  getLine = state getLine'
    where
      formatResult mockStdio (res, rest)
          | rest == [] = (res, mockStdio { mockStdin = [] })
          | otherwise  = (res, mockStdio { mockStdin = tail rest })
      getLine' mockStdio@(MockStdio stdin _)
          | stdin  == [] = ("", mockStdio { mockStdin = [] })
          | otherwise    = formatResult mockStdio $ break (== '\n') stdin

  getChar = state getChar'
    where
      getChar' mockStdio@(MockStdio stdin _)
          | stdin == [] = ('\0', mockStdio { mockStdin = [] })
          | otherwise   = (head stdin, mockStdio { mockStdin = tail stdin })

  print str = state print'
    where
      print' mockStdio = ((), mockStdio { mockStdout = (mockStdout mockStdio ++ str) })

makeMockStdio :: String -> MockStdio
makeMockStdio stdin = MockStdio { mockStdin = stdin, mockStdout = "" }
