module Stack
    ( Stack
    , pop
    , push
    , duplicate
    , swap
    ) where

import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state pop'
  where
    pop' []     = (0, [])
    pop' (x:xs) = (x, xs)

push :: Int -> State Stack ()
push = state . (\x xs -> ((), (x:xs)))

duplicate :: State Stack ()
duplicate = do
    v1 <- pop
    push v1
    push v1

swap :: State Stack ()
swap = do
    v1 <- pop
    v2 <- pop
    push v1
    push v2
