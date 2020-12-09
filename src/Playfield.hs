module Playfield
    ( Playfield (..)
    , standardWidth
    , standardHeight
    , Playfield.getChar
    , Playfield.putChar
    , fromString
    , emptyPlayfield
    ) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data Playfield = Playfield
    { playfieldWidth  :: !Int
    , playfieldHeight :: !Int
    , playfieldRaw    :: !(V.Vector Char)
    } deriving (Eq)

instance Show Playfield where
    show (Playfield w _ raw) = intercalate "\n" $ chunksOf w $ V.toList raw

standardWidth :: Int
standardWidth = 80

standardHeight :: Int
standardHeight = 25

validLocation :: Playfield -> (Int, Int) -> Bool
validLocation (Playfield w h _) (x, y) = x >= 0 && x < w && y >= 0 && y < h

getChar :: Playfield -> (Int, Int) -> Maybe Char
getChar playfield@(Playfield w  _ raw) loc@(x, y)
    | validLocation playfield loc = Just $ raw ! (y * w + x)
    | otherwise                   = Nothing

putChar :: Playfield -> (Int, Int) -> Char -> Playfield
putChar playfield@(Playfield w _ raw) (x, y) ch = playfield { playfieldRaw = newRaw }
  where
    newRaw
        | validLocation playfield (x, y) = V.modify (\v -> MV.write v (y * w + x) ch) raw
        | otherwise                      = raw

fromString :: Int -> Int -> String -> Playfield
fromString w h inputString = Playfield w h $ V.fromList raw
  where
    padToWidth line       = take w $ (take w line) ++ (repeat ' ')
    padToHeight linesList = take h $ linesList ++ (repeat $ take w (repeat ' '))
    raw                   = concat $ padToHeight $ map padToWidth (lines inputString)

emptyPlayfield :: Int -> Int -> Playfield
emptyPlayfield w h = Playfield w h (V.replicate (w * h) ' ')
