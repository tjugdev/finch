module Playfield
    ( Playfield (..)
    , standardWidth
    , standardHeight
    , Playfield.getChar
    , Playfield.putChar
    , fromString
    , emptyPlayfield
    ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed ((!))
import Data.List.Split (chunksOf)
import Data.List (intercalate)

data Playfield = Playfield
    { width :: !Int
    , height :: !Int
    , source :: !(V.Vector Char)
    } deriving (Eq)

instance Show Playfield where
    show (Playfield w h p) = intercalate "\n" $ chunksOf w $ V.toList p

standardWidth = 80 :: Int
standardHeight = 25 :: Int

validLocation :: Playfield -> (Int, Int) -> Bool
validLocation (Playfield w h _) (x, y) = x >= 0 && x < w && y >= 0 && y < h

getChar :: Playfield -> (Int, Int) -> Char
getChar pf@(Playfield w  h s) loc@(x, y)
    | validLocation pf loc = s ! (y * w + x)
    | otherwise = ' '

putChar :: Playfield -> (Int, Int) -> Char -> Playfield
putChar pf@(Playfield w h s) (x, y) ch = pf { source = newSource }
  where
    newSource
        | validLocation pf (x, y) = V.modify (\v -> MV.write v (y * w + x) ch) s
        | otherwise = s

fromString :: Int -> Int -> String -> Playfield
fromString w h s = Playfield w h $ V.fromList sourceData
  where
    padToWidth line = take w $ (take w line) ++ (repeat ' ')
    padToHeight lines = take h $ lines ++ (repeat $ take w (repeat ' '))
    sourceData = concat $ padToHeight $ map padToWidth (lines s)

emptyPlayfield :: Int -> Int -> Playfield
emptyPlayfield w h = Playfield w h (V.replicate (w * h) ' ')
