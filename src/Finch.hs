module Finch
    ( runString
    , runStringStandardSize
    ) where

import qualified Commands as Cmd
import Control.Monad (liftM2, liftM3)
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
    { statePlayfield   = pf
    , statePC          = (0, 0)
    , stateStack       = []
    , stateCurrentDirection = DirR
    , stateFinished    = False
    , stateStringMode  = False
    }

setDirection :: ProgramState -> Direction -> ProgramState
setDirection ps dir = ps { stateCurrentDirection = dir }

setStack :: ProgramState -> S.Stack -> ProgramState
setStack ps stack = ps { stateStack = stack }

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
modifyStack ps s = setStack ps $ execState s $ stateStack ps

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
    return $ setStack ps newStack

popPrintChar :: FinchIO m => ProgramState -> m ProgramState
popPrintChar ps = do
    let (top, newStack) = runState S.pop $ stateStack ps
    FIO.print $ [chr $ top `mod` 255]
    return $ setStack ps newStack

pushReadInteger :: FinchIO m => ProgramState -> m ProgramState
pushReadInteger ps = modifyStack ps <$> S.push .stringToInteger <$> FIO.getLine

pushReadChar :: FinchIO m => ProgramState -> m ProgramState
pushReadChar ps = modifyStack ps <$> S.push . ord <$> FIO.getChar

handleGet :: ProgramState -> ProgramState
handleGet ps = modifyStack ps $ getCharacter <$> liftM2 (,) S.pop S.pop >>= S.push
  where
    getCharacter (y, x) = maybe 0 ord $ P.getChar (statePlayfield ps) (x, y)

handlePut :: ProgramState -> ProgramState
handlePut ps = ps { stateStack = newStack, statePlayfield = newPlayfield }
  where
    ((y, x, val), newStack) =
        (flip runState) (stateStack ps) $ liftM3 (,,) S.pop S.pop S.pop
    newPlayfield = P.putChar (statePlayfield ps) (x, y) (chr val)

divisionByZeroPrompt :: Int -> String
divisionByZeroPrompt = printf "What do you want %ld/0 to be? "

handleDivisionLike :: FinchIO m => ProgramState -> (Int -> Int -> Int) -> m ProgramState
handleDivisionLike ps op = do
    val <- valueToPush
    let newStack = execState (S.push val) stackAfterPop
    return ps { stateStack = newStack }
  where
    (values, stackAfterPop) = (flip runState) (stateStack ps) $ liftM2 (,) S.pop S.pop
    valueToPush = case values of
                      (0, x) -> do
                          FIO.print $ divisionByZeroPrompt x
                          desiredResult <- FIO.getLine
                          return $ stringToInteger desiredResult
                      (y, x) -> return $ op x y

moveRandom :: FinchIO m => ProgramState -> m ProgramState
moveRandom ps = setDirection ps <$> toEnum <$> (flip mod 4) <$> FIO.random

toggleStringMode :: ProgramState -> ProgramState
toggleStringMode ps = ps { stateStringMode = (not . stateStringMode) ps }

applyStackOp :: ProgramState -> (Int -> Int -> Int) -> ProgramState
applyStackOp ps op = modifyStack ps $ liftM2 (flip op) S.pop S.pop >>= S.push

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
                (\x -> if x == 0 then 1 else 0) <$> S.pop >>= S.push
            Cmd.GreaterThan -> applyStackOp ps (\x y -> if x > y then 1 else 0)
            Cmd.MoveRight -> setDirection ps DirR
            Cmd.MoveLeft -> setDirection ps DirL
            Cmd.MoveUp -> setDirection ps DirU
            Cmd.MoveDown -> setDirection ps DirD
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
    ch            = getCurrentChar ps
    newStringMode = ch /= Cmd.StringMode
    oldStack      = stateStack ps
    newStack      = if newStringMode
                       then execState (S.push $ ord ch) oldStack
                       else oldStack

processCurrentChar :: FinchIO m => ProgramState -> m ProgramState
processCurrentChar ps = if stateStringMode ps
                           then return $ handleStringMode ps
                           else processCurrentCmd ps

step :: FinchIO m => ProgramState -> m ProgramState
step = (fmap advancePC) . processCurrentChar

run :: FinchIO m => ProgramState -> m ProgramState
run = iterateUntilM stateFinished step

runString :: FinchIO m => Int -> Int -> String -> m ProgramState
runString width height input = run programState
  where
    playfield    = P.fromString width height input
    programState = initialProgramState playfield

runStringStandardSize :: FinchIO m => String -> m ProgramState
runStringStandardSize = runString P.standardWidth P.standardHeight
