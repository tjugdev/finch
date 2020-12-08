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

add :: State Stack ()
add = do
    v1 <- pop
    v2 <- pop
    push (v2 + v1)

subtract :: State Stack ()
subtract = do
    v1 <- pop
    v2 <- pop
    push (v2 - v1)

multiply :: State Stack ()
multiply = do
    v1 <- pop
    v2 <- pop
    push (v2 * v1)

-- TODO Division by zero?
divide :: State Stack ()
divide = do
    v1 <- pop
    v2 <- pop
    push (v2 `div` v1)

-- TODO Division by zero?
modulo :: State Stack ()
modulo = do
    v1 <- pop
    v2 <- pop
    push (v2 `mod` v1)

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

not :: State Stack ()
not = do
    v1 <- pop
    push $ if v1 == 0 then 1 else 0

greaterThan :: State Stack ()
greaterThan = do
    v1 <- pop
    v2 <- pop
    push $ if v2 > v1 then 1 else 0
