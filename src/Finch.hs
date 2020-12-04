module Finch where

import qualified Commands as Cmd
import qualified Stack
import qualified Data.Vector.Unboxed as Vec
import Data.Vector.Unboxed ((!), Vector)
import Data.List.Split (chunksOf)
import Data.List (intercalate)
import Control.Monad.State (State, execState, runState)
import Control.Monad.Loops (iterateUntilM)
import Data.Char (chr, ord, isDigit, digitToInt)

type PC = (Int, Int)

-- Playfield
data Playfield = Playfield
    { width :: !Int
    , height :: !Int
    , source :: !(Vector Char)
    } deriving (Eq)

instance Show Playfield where
    show (Playfield w h p) = intercalate "\n" $ chunksOf w $ Vec.toList p

playfieldHeight = 25
playfieldWidth = 80

emptyPlayfield = Playfield
    playfieldWidth
    playfieldHeight
    (Vec.replicate (playfieldWidth * playfieldHeight) ' ')

validLocation :: PC -> Playfield -> Bool
validLocation (x, y) (Playfield w h _) = x >= 0 && x < w && y >= 0 && y < h

getCharAt :: PC -> Playfield -> Char
getCharAt pc@(x, y) pf@(Playfield w  h source)
    | validLocation pc pf = source ! (y * w + x)
    | otherwise = ' '

playfieldFromString :: String -> Int -> Int -> Playfield
playfieldFromString source w h = Playfield w h $ Vec.fromList sourceData
  where
    padToWidth line = take w $ (take w line) ++ (repeat ' ')
    padToHeight lines = take h $ lines ++ (repeat $ take w (repeat ' '))
    sourceData = concat $ padToHeight $ map padToWidth (lines source)

-- Interpreter
data Direction = DirL | DirR | DirU | DirD deriving (Show, Eq)

data ProgramState = ProgramState
    { playfield :: !Playfield
    , pc :: !PC
    , stack :: !Stack.Stack
    , currentDirection :: !Direction
    , finished :: !Bool
    , stringMode :: !Bool
    } deriving (Show, Eq)

initialProgramState :: Playfield -> ProgramState
initialProgramState pf = ProgramState
    { playfield = pf
    , pc = (0, 0)
    , stack = []
    , currentDirection = DirR
    , finished = False
    , stringMode  = False
    }

getCurrentChar :: ProgramState -> Char
getCurrentChar ps = getCharAt (pc ps) (playfield ps)

advancePC :: ProgramState -> ProgramState
advancePC ps = ps { pc = newPC }
  where
    (x, y) = pc ps
    w = width $ playfield ps
    h = height $ playfield ps
    newPC = case currentDirection ps of
                DirL -> (x - 1 `mod` w, y)
                DirR -> (x + 1 `mod` w, y)
                DirU -> (x, y - 1 `mod` h)
                DirD -> (x, y + 1 `mod` h)

modifyStack :: ProgramState -> (State Stack.Stack a) -> ProgramState
modifyStack ps s = let newStack = execState s $ stack ps in ps { stack = newStack }

directionalIf :: (Direction, Direction) -> ProgramState -> ProgramState
directionalIf (d1, d2) ps = ps
    { currentDirection = if topZero then d1 else d2
    , stack = newStack
    }
  where
    (topZero, newStack) = runState (fmap (== 0) Stack.pop) (stack ps)

horizontalIf :: ProgramState -> ProgramState
horizontalIf = directionalIf (DirR, DirL)

verticalIf :: ProgramState -> ProgramState
verticalIf = directionalIf (DirD, DirU)

popPrintInteger :: ProgramState -> IO ProgramState
popPrintInteger ps = do
    let (top, newStack) = runState Stack.pop $ stack ps
    putStr $ show top
    return ps { stack = newStack }

popPrintChar :: ProgramState -> IO ProgramState
popPrintChar ps = do
    let (top, newStack) = runState Stack.pop $ stack ps
    putStr $ [chr top]
    return ps { stack = newStack }

pushReadInteger :: ProgramState -> IO ProgramState
pushReadInteger ps = do
    str <- getLine
    return $ modifyStack ps (Stack.push $ read str)

pushReadChar :: ProgramState -> IO ProgramState
pushReadChar ps = do
    ch <- getChar
    return $ modifyStack ps (Stack.push $ ord ch)

handleGet :: ProgramState -> ProgramState
handleGet ps = modifyStack ps $ do
    y <- Stack.pop
    x <- Stack.pop
    Stack.push $ ord $ getCharAt (x, y) (playfield ps)

handleNonIOCmd :: ProgramState -> ProgramState
handleNonIOCmd ps = case cmd of
    Cmd.Noop -> ps
    Cmd.Plus -> modifyStack ps Stack.add
    Cmd.Minus -> modifyStack ps Stack.subtract
    Cmd.Mult -> modifyStack ps Stack.multiply
    Cmd.Div -> modifyStack ps Stack.divide
    Cmd.Mod -> modifyStack ps Stack.modulo
    Cmd.Not -> modifyStack ps Stack.not
    Cmd.GreaterThan -> modifyStack ps Stack.greaterThan
    Cmd.MoveRight -> ps { currentDirection = DirR }
    Cmd.MoveLeft -> ps { currentDirection = DirL }
    Cmd.MoveUp -> ps { currentDirection = DirU }
    Cmd.MoveDown -> ps { currentDirection = DirD }
    Cmd.HorizontalIf -> horizontalIf ps
    Cmd.VerticalIf -> verticalIf ps
    Cmd.StringMode -> ps -- TODO
    Cmd.Duplicate -> modifyStack ps Stack.duplicate
    Cmd.Swap -> modifyStack ps Stack.swap
    Cmd.PopDiscard -> modifyStack ps Stack.pop
    Cmd.Bridge -> advancePC ps
    Cmd.Put -> ps -- TODO
    Cmd.Get -> handleGet ps
    Cmd.Halt -> ps { finished = True }
    _ -> if isDigit cmd
            then modifyStack ps (Stack.push $ digitToInt cmd)
            else ps
  where
    cmd = getCurrentChar ps

processCurrentChar :: ProgramState -> IO ProgramState
processCurrentChar ps = do
    let cmd = getCurrentChar ps
    case cmd of
        Cmd.PopPrintInteger -> popPrintInteger ps
        Cmd.PopPrintChar -> popPrintChar ps
        Cmd.PromptInteger -> pushReadInteger ps
        Cmd.PromptChar -> pushReadChar ps
        --Cmd.MoveRandom -> ps -- TODO
        _ -> return $ handleNonIOCmd ps

step :: ProgramState -> IO ProgramState
step = (fmap advancePC) . processCurrentChar

run :: ProgramState -> IO ProgramState
run = iterateUntilM finished step
