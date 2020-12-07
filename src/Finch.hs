module Finch where

import qualified Commands as Cmd
import qualified Stack
import qualified Playfield
import Control.Monad.State (State, execState, runState)
import Control.Monad.Loops (iterateUntilM)
import Data.Char (chr, ord, isDigit, digitToInt)
import System.Random (randomIO)

type PC = (Int, Int)

-- Interpreter
data Direction = DirL | DirR | DirU | DirD deriving (Show, Eq, Enum)

data ProgramState = ProgramState
    { playfield :: !Playfield.Playfield
    , pc :: !PC
    , stack :: !Stack.Stack
    , currentDirection :: !Direction
    , finished :: !Bool
    , stringMode :: !Bool
    } deriving (Show, Eq)

initialProgramState :: Playfield.Playfield -> ProgramState
initialProgramState pf = ProgramState
    { playfield = pf
    , pc = (0, 0)
    , stack = []
    , currentDirection = DirR
    , finished = False
    , stringMode  = False
    }

getCurrentChar :: ProgramState -> Char
getCurrentChar ps = Playfield.getChar (playfield ps) (pc ps)

advancePC :: ProgramState -> ProgramState
advancePC ps = ps { pc = newPC }
  where
    (x, y) = pc ps
    w = Playfield.width $ playfield ps
    h = Playfield.height $ playfield ps
    newPC = case currentDirection ps of
                DirL -> (x - 1 `mod` w, y)
                DirR -> (x + 1 `mod` w, y)
                DirU -> (x, y - 1 `mod` h)
                DirD -> (x, y + 1 `mod` h)

modifyStack :: ProgramState -> (State Stack.Stack a) -> ProgramState
modifyStack ps s = ps { stack = newStack }
  where
    newStack = execState s $ stack ps

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
    Stack.push $ ord $ Playfield.getChar (playfield ps) (x, y)

handlePut :: ProgramState -> ProgramState
handlePut ps = ps { stack = newStack, playfield = newPlayfield }
  where
    stackAction = do
        y <- Stack.pop
        x <- Stack.pop
        val <- Stack.pop
        return ((x, y), val)
    (((x, y), val), newStack) = runState stackAction $ stack ps
    pf = playfield ps;
    newPlayfield = Playfield.putChar pf (x, y) (chr val)

moveRandom :: ProgramState -> IO ProgramState
moveRandom ps = do
    r <- randomIO :: IO Int
    let direction = toEnum (r `mod` 4)
    return $ ps { currentDirection = direction }

toggleStringMode :: ProgramState -> ProgramState
toggleStringMode ps = ps { stringMode = (not . stringMode) ps }

processCurrentCmd :: ProgramState -> IO ProgramState
processCurrentCmd ps = case cmd of
    Cmd.PopPrintInteger -> popPrintInteger ps
    Cmd.PopPrintChar -> popPrintChar ps
    Cmd.PromptInteger -> pushReadInteger ps
    Cmd.PromptChar -> pushReadChar ps
    Cmd.MoveRandom -> moveRandom ps
    _ -> return $
        case cmd of
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
            Cmd.StringMode -> toggleStringMode ps
            Cmd.Duplicate -> modifyStack ps Stack.duplicate
            Cmd.Swap -> modifyStack ps Stack.swap
            Cmd.PopDiscard -> modifyStack ps Stack.pop
            Cmd.Bridge -> advancePC ps
            Cmd.Put -> handlePut ps
            Cmd.Get -> handleGet ps
            Cmd.Halt -> ps { finished = True }
            _ -> if isDigit cmd
                    then modifyStack ps (Stack.push $ digitToInt cmd)
                    else ps
  where
    cmd = getCurrentChar ps

handleStringMode :: ProgramState -> ProgramState
handleStringMode ps = ps
    { stringMode = newStringMode
    , stack = newStack
    }
  where
    ch = Playfield.getChar (playfield ps) (pc ps)
    newStringMode = ch /= Cmd.StringMode
    newStack
        | newStringMode = execState (Stack.push $ ord ch) (stack ps)
        | otherwise = stack ps

processCurrentChar :: ProgramState -> IO ProgramState
processCurrentChar ps
    | stringMode ps = return $ handleStringMode ps
    | otherwise = processCurrentCmd ps

step :: ProgramState -> IO ProgramState
step = (fmap advancePC) . processCurrentChar

run :: ProgramState -> IO ProgramState
run = iterateUntilM finished step

runString :: Int -> Int -> String -> IO ProgramState
runString width height input = do
    let playfield = Playfield.fromString width height input
        programState = initialProgramState playfield
    run programState
