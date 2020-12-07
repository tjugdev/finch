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
    , raw :: !(V.Vector Char)
    } deriving (Eq)

instance Show Playfield where
    show (Playfield w h raw) = intercalate "\n" $ chunksOf w $ V.toList raw

standardWidth = 80 :: Int
standardHeight = 25 :: Int

validLocation :: Playfield -> (Int, Int) -> Bool
validLocation (Playfield w h _) (x, y) = x >= 0 && x < w && y >= 0 && y < h

getChar :: Playfield -> (Int, Int) -> Char
getChar playfield@(Playfield w  h raw) loc@(x, y)
    | validLocation playfield loc = raw ! (y * w + x)
    | otherwise = ' '

putChar :: Playfield -> (Int, Int) -> Char -> Playfield
putChar playfield@(Playfield w h raw) (x, y) ch = playfield { raw = newRaw }
  where
    newRaw
        | validLocation playfield (x, y) = V.modify (\v -> MV.write v (y * w + x) ch) raw
        | otherwise = raw

fromString :: Int -> Int -> String -> Playfield
fromString w h inputString = Playfield w h $ V.fromList raw
  where
    padToWidth line = take w $ (take w line) ++ (repeat ' ')
    padToHeight lines = take h $ lines ++ (repeat $ take w (repeat ' '))
    raw = concat $ padToHeight $ map padToWidth (lines inputString)

emptyPlayfield :: Int -> Int -> Playfield
emptyPlayfield w h = Playfield w h (V.replicate (w * h) ' ')
