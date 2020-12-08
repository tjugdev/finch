module Finch
    ( runString
    , runStringStandardSize
    , runStringStandardSize_
    ) where

import qualified Commands as Cmd
import qualified Stack
import qualified Playfield as P
--import qualified FinchIO
import Control.Monad.State (State, execState, runState)
import Control.Monad.Loops (iterateUntilM)
import Data.Char (chr, ord, isDigit, digitToInt)
import System.Random (randomIO)

type PC = (Int, Int)

-- Interpreter
data Direction = DirL | DirR | DirU | DirD deriving (Show, Eq, Enum)

data ProgramState = ProgramState
    { statePlayfield :: !P.Playfield
    , statePC :: !PC
    , stateStack :: !Stack.Stack
    , stateCurrentDirection :: !Direction
    , stateFinished :: !Bool
    , stateStringMode :: !Bool
    } deriving (Show, Eq)

initialProgramState :: P.Playfield -> ProgramState
initialProgramState pf = ProgramState
    { statePlayfield = pf
    , statePC = (0, 0)
    , stateStack = []
    , stateCurrentDirection = DirR
    , stateFinished = False
    , stateStringMode  = False
    }

getCurrentChar :: ProgramState -> Char
getCurrentChar ps = P.getChar (statePlayfield ps) (statePC ps)

advancePC :: ProgramState -> ProgramState
advancePC ps = ps { statePC = newPC }
  where
    (x, y) = statePC ps
    w = P.playfieldWidth $ statePlayfield ps
    h = P.playfieldHeight $ statePlayfield ps
    newPC = case stateCurrentDirection ps of
                DirL -> (x - 1 `mod` w, y)
                DirR -> (x + 1 `mod` w, y)
                DirU -> (x, y - 1 `mod` h)
                DirD -> (x, y + 1 `mod` h)

modifyStack :: ProgramState -> (State Stack.Stack a) -> ProgramState
modifyStack ps s = ps { stateStack = newStack }
  where
    newStack = execState s $ stateStack ps

directionalIf :: (Direction, Direction) -> ProgramState -> ProgramState
directionalIf (d1, d2) ps = ps
    { stateCurrentDirection = if topZero then d1 else d2
    , stateStack = newStack
    }
  where
    (topZero, newStack) = runState (fmap (== 0) Stack.pop) (stateStack ps)

horizontalIf :: ProgramState -> ProgramState
horizontalIf = directionalIf (DirR, DirL)

verticalIf :: ProgramState -> ProgramState
verticalIf = directionalIf (DirD, DirU)

popPrintInteger :: ProgramState -> IO ProgramState
popPrintInteger ps = do
    let (top, newStack) = runState Stack.pop $ stateStack ps
    putStr $ show top
    return ps { stateStack = newStack }

popPrintChar :: ProgramState -> IO ProgramState
popPrintChar ps = do
    let (top, newStack) = runState Stack.pop $ stateStack ps
    putStr $ [chr top]
    return ps { stateStack = newStack }

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
    Stack.push $ ord $ P.getChar (statePlayfield ps) (x, y)

handlePut :: ProgramState -> ProgramState
handlePut ps = ps { stateStack = newStack, statePlayfield = newPlayfield }
  where
    stackAction = do
        yVal <- Stack.pop
        xVal <- Stack.pop
        putVal <- Stack.pop
        return ((xVal, yVal), putVal)
    (((x, y), val), newStack) = runState stackAction $ stateStack ps
    pf = statePlayfield ps;
    newPlayfield = P.putChar pf (x, y) (chr val)

moveRandom :: ProgramState -> IO ProgramState
moveRandom ps = do
    r <- randomIO :: IO Int
    let direction = toEnum (r `mod` 4)
    return $ ps { stateCurrentDirection = direction }

toggleStringMode :: ProgramState -> ProgramState
toggleStringMode ps = ps { stateStringMode = (not . stateStringMode) ps }

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
            Cmd.MoveRight -> ps { stateCurrentDirection = DirR }
            Cmd.MoveLeft -> ps { stateCurrentDirection = DirL }
            Cmd.MoveUp -> ps { stateCurrentDirection = DirU }
            Cmd.MoveDown -> ps { stateCurrentDirection = DirD }
            Cmd.HorizontalIf -> horizontalIf ps
            Cmd.VerticalIf -> verticalIf ps
            Cmd.StringMode -> toggleStringMode ps
            Cmd.Duplicate -> modifyStack ps Stack.duplicate
            Cmd.Swap -> modifyStack ps Stack.swap
            Cmd.PopDiscard -> modifyStack ps Stack.pop
            Cmd.Bridge -> advancePC ps
            Cmd.Put -> handlePut ps
            Cmd.Get -> handleGet ps
            Cmd.Halt -> ps { stateFinished = True }
            _ -> if isDigit cmd
                    then modifyStack ps (Stack.push $ digitToInt cmd)
                    else ps
  where
    cmd = getCurrentChar ps

handleStringMode :: ProgramState -> ProgramState
handleStringMode ps = ps
    { stateStringMode = newStringMode
    , stateStack = newStack
    }
  where
    ch = P.getChar (statePlayfield ps) (statePC ps)
    newStringMode = ch /= Cmd.StringMode
    newStack
        | newStringMode = execState (Stack.push $ ord ch) (stateStack ps)
        | otherwise = stateStack ps

processCurrentChar :: ProgramState -> IO ProgramState
processCurrentChar ps
    | stateStringMode ps = return $ handleStringMode ps
    | otherwise = processCurrentCmd ps

step :: ProgramState -> IO ProgramState
step = (fmap advancePC) . processCurrentChar

run :: ProgramState -> IO ProgramState
run = iterateUntilM stateFinished step

runString :: Int -> Int -> String -> IO ProgramState
runString width height input = do
    let playfield = P.fromString width height input
        programState = initialProgramState playfield
    run programState

runStringStandardSize :: String -> IO ProgramState
runStringStandardSize = runString P.standardWidth P.standardHeight

runStringStandardSize_ :: String -> IO ()
runStringStandardSize_ inputString = runStringStandardSize inputString >> return ()
