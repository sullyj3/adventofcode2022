{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE MultiWayIf #-}
module Day12 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import           Data.Foldable     (minimum)
import           Data.Massiv.Array (Ix2 (..), P, Sz (..), (!), MMatrix, B)
import qualified Data.Massiv.Array as A
import           Data.Massiv.Core  (Matrix)
import           PyF               (str)
import           Relude.Unsafe     (fromJust)
import qualified Data.Set as Set
import List.Transformer (ListT, select, runListT)
import Data.Traversable (for)
import Control.Monad.ST
import Data.List (minimumBy)
import Control.Monad (foldM)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MQ


-- |~) _  _ _. _  _
-- |~ (_|| _\|| |(_|
--                _|
-- >>> parseInput $ exampleInput
parseInput ∷ Text → Matrix P Char
parseInput = A.fromLists' A.Seq . map toString . lines

canMoveFromTo :: Char -> Char -> Bool
canMoveFromTo c1 c2 = ord (convert c2) <= ord (convert c1) + 1
  where
  convert c | c == 'S' = 'a'
            | c == 'E' = 'z'
            | otherwise = c


-- |~) _  __|_  /~\ _  _
-- |~ (_||  |   \_/| |(/_
--
-- >>> part1 . parseInput $ exampleInput
type Path = [Ix2]

part1 ∷ Matrix P Char → Int
part1 heightmap = minimum . map length $ paths
  where
    start = fromJust $ A.findIndex (== 'S') heightmap
    Sz (height:.width) = A.size heightmap

    inBounds (i:.j) = i >= 0 && j >= 0 && i < height && j < width
    neighbours ix = do
      dix <- [-1:.0, 1:.0, 0:.1, 0:.(-1)]
      let ix' = ix + dix
      guard $ inBounds ix'
      guard $ canMoveFromTo (heightmap ! ix) (heightmap ! ix')
      pure ix'

    -- Probably correct, but too slow
    paths :: [Path]
    paths = visit [] start

    -- dfs
    visit ∷ Path -> Ix2 → [Path]
    visit path ix 
      | here == 'E' = [reverse path]
      | otherwise = concatMap (visit path') . filter canMoveTo $ neighbours ix
      where
        here = heightmap ! ix
        path' = ix : path
        
        canMoveTo neighbour = canMoveFromTo here (heightmap ! neighbour) 
                           && neighbour `notElem` path


-- |~) _  __|_  ~|~  _
-- |~ (_||  |    |VV(_)
--
-- >>> part2 . parseInput $ exampleInput
part2 ∷ a → a
part2 = id


-- |\/| _ . _
-- |  |(_||| |
--
main ∷ IO ()
main = do
  -- other testing here
  -- aocSinglePartExample parseInput part1 exampleInput
  aocSinglePartMain "inputs/12.txt" exampleInput parseInput part1
  -- aocMain "inputs/12.txt" Solution { parse=parseInput, part1=part1, part2=part2 }


-- (~   _  _ _  _ | _   . _  _   _|_
-- (_><(_|| | ||_)|(/_  || ||_)|_||
--             |            |
exampleInput ∷ Text
exampleInput = toText @String [str|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|]

smallExample ∷ Text
smallExample = toText @String [str|Sab
mno
xyE|]
