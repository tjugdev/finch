module Stack
    ( Stack
    , pop
    , push
    , add
    , Stack.subtract
    , multiply
    , divide
    , modulo
    , duplicate
    , swap
    , Stack.not
    , greaterThan
    ) where

import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state pop'
  where
    pop' []     = (0, [])
    pop' (x:xs) = (x, xs)

push :: Int -> State Stack ()
push = state . push'
  where
    push' x xs = ((), (x:xs))

add :: State Stack Int
add = do
    v1 <- pop
    v2 <- pop
    return $ v2 + v1

subtract :: State Stack Int
subtract = do
    v1 <- pop
    v2 <- pop
    return $ v2 - v1

multiply :: State Stack Int
multiply = do
    v1 <- pop
    v2 <- pop
    return $ v2 * v1

divide :: State Stack (Maybe Int)
divide = do
    v1 <- pop
    v2 <- pop
    return $ if v1 == 0 then Nothing else Just (v2 `div` v1)

modulo :: State Stack (Maybe Int)
modulo = do
    v1 <- pop
    v2 <- pop
    return $ if v1 == 0 then Nothing else Just (v2 `mod` v1)

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

not :: State Stack Int
not = do
    v1 <- pop
    return $ if v1 == 0 then 1 else 0

greaterThan :: State Stack Int
greaterThan = do
    v1 <- pop
    v2 <- pop
    return $ if v2 > v1 then 1 else 0
