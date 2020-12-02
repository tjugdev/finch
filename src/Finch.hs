module Finch where

import qualified Commands as Cmd
import qualified Data.Vector.Unboxed as Vec
import Data.Vector.Unboxed ((!), Vector)
import Data.List.Split (chunksOf)
import Data.List (intercalate)

type Command = Char
type Stack = [Int]
type PC = (Int, Int)
data Playfield = Playfield
    { width :: Int
    , height :: Int
    , source :: (Vector Command)
    } deriving (Eq)

-- Playfield
instance Show Playfield where
    show (Playfield w h p) = intercalate "\n" $ chunksOf w $ Vec.toList p

playfieldHeight = 25
playfieldWidth = 80

emptyPlayfield = Playfield playfieldWidth playfieldHeight (Vec.replicate (playfieldWidth * playfieldHeight) ' ')

validLocation :: PC -> Playfield -> Bool
validLocation (x, y) (Playfield w h _) = x >= 0 && x < w && y <= 0 && y < h

getCharAt :: PC -> Playfield -> Char
getCharAt pc@(x, y) pf@(Playfield w  h source)
    | validLocation pc pf = source ! (y * h + x)
    | otherwise = ' '

playfieldFromString :: String -> Int -> Int -> Playfield
playfieldFromString source w h = Playfield w h $ Vec.fromList sourceData
    where padToWidth line = take w $ (take w line) ++ (repeat ' ')
          padToHeight lines = take h $ lines ++ (repeat $ take w (repeat ' '))
          sourceData = concat $ padToHeight $ map padToWidth (lines source)

-- Interpreter
data Direction = DirL | DirR | DirU | DirD

data ProgramState = ProgramState
    { playfield :: Playfield
    , pc :: PC
    , stack :: Stack
    , currentDirection :: Direction
    }

initialProgramState :: Playfield -> ProgramState
initialProgramState pf = ProgramState
    { playfield = pf
    , pc = (0, 0)
    , stack = []
    , currentDirection = DirR
    }

pop :: Stack -> (Int, Stack)
pop [] = (0, [])
pop (x:xs) = (x, xs)

push :: Stack -> Int -> Stack
push xs x = (x:xs)

getCurrentCommand :: ProgramState -> Command
getCurrentCommand ps = getCharAt (pc ps) (playfield ps)
