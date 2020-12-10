module Finch
    ( runString
    , runStringStandardSize
    , runStringStandardSize_
    ) where

import qualified Commands as Cmd
import Control.Applicative (liftA2)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State (State, execState, runState)
import Data.Char (chr, digitToInt, isDigit, ord)
import Data.Maybe (fromJust)
import FinchIO (FinchIO)
import qualified FinchIO as FIO
import qualified Playfield as P
import qualified Stack as S
import Text.Printf (printf)
import Text.Read (readMaybe)

type PC = (Int, Int)

-- Interpreter
data Direction = DirL | DirR | DirU | DirD deriving (Show, Eq, Enum)

data ProgramState = ProgramState
    { statePlayfield        :: !P.Playfield
    , statePC               :: !PC
    , stateStack            :: !S.Stack
    , stateCurrentDirection :: !Direction
    , stateFinished         :: !Bool
    , stateStringMode       :: !Bool
    } deriving (Show, Eq)

initialProgramState :: P.Playfield -> ProgramState
initialProgramState pf = ProgramState
    { statePlayfield  = pf
    , statePC         = (0, 0)
    , stateStack      = []
    , stateCurrentDirection = DirR
    , stateFinished   = False
    , stateStringMode = False
    }

getCurrentChar :: ProgramState -> Char
getCurrentChar ps = fromJust $ P.getChar (statePlayfield ps) (statePC ps)

advancePC :: ProgramState -> ProgramState
advancePC ps = ps { statePC = newPC }
  where
    (x, y) = statePC ps
    w      = P.playfieldWidth $ statePlayfield ps
    h      = P.playfieldHeight $ statePlayfield ps
    newPC  = case stateCurrentDirection ps of
                 DirL -> ((x - 1) `mod` w, y)
                 DirR -> ((x + 1) `mod` w, y)
                 DirU -> (x, (y - 1) `mod` h)
                 DirD -> (x, (y + 1) `mod` h)

stringToInteger :: String -> Int
stringToInteger str = maybe 0 truncate (readMaybe str :: Maybe Double)

modifyStack :: ProgramState -> (State S.Stack a) -> ProgramState
modifyStack ps s = ps { stateStack = newStack }
  where
    newStack = execState s $ stateStack ps

directionalIf :: (Direction, Direction) -> ProgramState -> ProgramState
directionalIf (d1, d2) ps = ps
    { stateCurrentDirection = if topZero then d1 else d2
    , stateStack            = newStack
    }
  where
    (topZero, newStack) = runState (fmap (== 0) S.pop) (stateStack ps)

horizontalIf :: ProgramState -> ProgramState
horizontalIf = directionalIf (DirR, DirL)

verticalIf :: ProgramState -> ProgramState
verticalIf = directionalIf (DirD, DirU)

popPrintInteger :: FinchIO m => ProgramState -> m ProgramState
popPrintInteger ps = do
    let (top, newStack) = runState S.pop $ stateStack ps
    FIO.print $ show top ++ " "
    return ps { stateStack = newStack }

popPrintChar :: FinchIO m => ProgramState -> m ProgramState
popPrintChar ps = do
    let (top, newStack) = runState S.pop $ stateStack ps
    FIO.print $ [chr $ top `mod` 255]
    return ps { stateStack = newStack }

pushReadInteger :: FinchIO m => ProgramState -> m ProgramState
pushReadInteger ps = do
    FIO.flushOutputBuffer
    str <- FIO.getLine
    return $ modifyStack ps (S.push $ stringToInteger str)

pushReadChar :: FinchIO m => ProgramState -> m ProgramState
pushReadChar ps = do
    FIO.flushOutputBuffer
    ch <- FIO.getChar
    return $ modifyStack ps (S.push $ ord ch)

handleGet :: ProgramState -> ProgramState
handleGet ps = modifyStack ps $ do
    y <- S.pop
    x <- S.pop
    S.push $ maybe 0 ord $ P.getChar (statePlayfield ps) (x, y)

handlePut :: ProgramState -> ProgramState
handlePut ps = ps { stateStack = newStack, statePlayfield = newPlayfield }
  where
    stackAction = do
        yVal <- S.pop
        xVal <- S.pop
        putVal <- S.pop
        return ((xVal, yVal), putVal)
    (((x, y), val), newStack) = runState stackAction $ stateStack ps
    pf = statePlayfield ps;
    newPlayfield = P.putChar pf (x, y) (chr val)

divisionByZeroPrompt :: Int -> String
divisionByZeroPrompt = printf "What do you want %ld/0 to be? "

handleDivisionLike
    :: FinchIO m
    => ProgramState
    -> (Int -> Int -> Int)
    -> m ProgramState
handleDivisionLike ps op = do
    val <- valueToPush
    let newStack = execState (S.push val) stackAfterPop
    return ps { stateStack = newStack }
  where
    (res, stackAfterPop) = (flip runState) (stateStack ps) $ do
        v1 <- S.pop
        v2 <- S.pop
        let result = if v1 == 0 then Nothing else Just (op v2 v1)
        return $ (result, v2, v1)
    valueToPush = case res of
                      (Just n, _, _) -> return n
                      (Nothing, x, _) -> do
                          FIO.print $ divisionByZeroPrompt x
                          FIO.flushOutputBuffer
                          desiredResult <- FIO.getLine
                          return $ stringToInteger desiredResult

moveRandom :: FinchIO m => ProgramState -> m ProgramState
moveRandom ps = do
    r <- FIO.random
    let direction = toEnum (r `mod` 4)
    return $ ps { stateCurrentDirection = direction }

toggleStringMode :: ProgramState -> ProgramState
toggleStringMode ps = ps { stateStringMode = (not . stateStringMode) ps }

applyStackOp :: ProgramState -> (Int -> Int -> Int) -> ProgramState
applyStackOp ps op = modifyStack ps $ liftA2 (flip op) S.pop S.pop >>= S.push

processCurrentCmd :: FinchIO m => ProgramState -> m ProgramState
processCurrentCmd ps = case cmd of
    Cmd.PopPrintInteger -> popPrintInteger ps
    Cmd.PopPrintChar -> popPrintChar ps
    Cmd.PromptInteger -> pushReadInteger ps
    Cmd.PromptChar -> pushReadChar ps
    Cmd.MoveRandom -> moveRandom ps
    Cmd.Div -> handleDivisionLike ps div
    -- Reference implementation actualy crashes when computing n % 0
    -- We choose to handle it the same as division by 0
    Cmd.Mod -> handleDivisionLike ps mod
    _ -> return $
        case cmd of
            Cmd.Noop -> ps
            Cmd.Plus -> applyStackOp ps (+)
            Cmd.Minus -> applyStackOp ps (-)
            Cmd.Mult -> applyStackOp ps (*)
            Cmd.Not -> modifyStack ps $
                (\x -> if x == 0 then 1 else 0) <$> S.pop  >>= S.push
            Cmd.GreaterThan -> applyStackOp ps (\x y -> if x > y then 1 else 0)
            Cmd.MoveRight -> ps { stateCurrentDirection = DirR }
            Cmd.MoveLeft -> ps { stateCurrentDirection = DirL }
            Cmd.MoveUp -> ps { stateCurrentDirection = DirU }
            Cmd.MoveDown -> ps { stateCurrentDirection = DirD }
            Cmd.HorizontalIf -> horizontalIf ps
            Cmd.VerticalIf -> verticalIf ps
            Cmd.StringMode -> toggleStringMode ps
            Cmd.Duplicate -> modifyStack ps S.duplicate
            Cmd.Swap -> modifyStack ps S.swap
            Cmd.PopDiscard -> modifyStack ps S.pop
            Cmd.Bridge -> advancePC ps
            Cmd.Put -> handlePut ps
            Cmd.Get -> handleGet ps
            Cmd.Halt -> ps { stateFinished = True }
            _ -> if isDigit cmd
                    then modifyStack ps (S.push $ digitToInt cmd)
                    else ps
  where
    cmd = getCurrentChar ps

handleStringMode :: ProgramState -> ProgramState
handleStringMode ps = ps
    { stateStringMode = newStringMode
    , stateStack      = newStack
    }
  where
    ch            = fromJust $ P.getChar (statePlayfield ps) (statePC ps)
    newStringMode = ch /= Cmd.StringMode
    newStack
        | newStringMode = execState (S.push $ ord ch) (stateStack ps)
        | otherwise     = stateStack ps

processCurrentChar :: FinchIO m => ProgramState -> m ProgramState
processCurrentChar ps
    | stateStringMode ps = return $ handleStringMode ps
    | otherwise          = processCurrentCmd ps

step :: FinchIO m => ProgramState -> m ProgramState
step = (fmap advancePC) . processCurrentChar

run :: FinchIO m => ProgramState -> m ProgramState
run = iterateUntilM stateFinished step

runString :: FinchIO m => Int -> Int -> String -> m ProgramState
runString width height input = do
    let playfield    = P.fromString width height input
        programState = initialProgramState playfield
    run programState

runStringStandardSize :: FinchIO m => String -> m ProgramState
runStringStandardSize = runString P.standardWidth P.standardHeight

runStringStandardSize_ :: FinchIO m => String -> m ()
runStringStandardSize_ inputString = runStringStandardSize inputString >> return ()
