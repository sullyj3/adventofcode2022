{-# LANGUAGE TupleSections #-}
module Day08 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import           Data.Char            (digitToInt)
import           Data.List            (maximum)
import qualified Data.Massiv.Array    as A
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as Vec
import           Prelude              hiding (some)
import           Text.Megaparsec.Char (digitChar)
import           Utils                (count, mapWithState, zipA)
import Data.Massiv.Array (Array, Ix2, U, Comp (..))
import PyF (str)

type Forest r = Array r Ix2 Int


-------------
-- Parsing --
-------------
parseInput ∷ Text → Forest U
parseInput = A.fromLists' (ParOn []) . unsafeParse (linesOf (some $ digitToInt <$> digitChar))

---------------
-- Solutions --
---------------

--
-- Part 1
--
part1 ∷ Forest U → Forest U
part1 = id
-- part1 = sum . map (count id) . allVisible

-- isVisibleFromLeft ∷ [Int] → [Bool]
-- isVisibleFromLeft = mapWithState (\n currMax -> (currMax < n, max n currMax)) minBound

-- isVisibleFromEitherSide ∷ [Int] → [Bool]
-- isVisibleFromEitherSide ns = zipWith (||)
--   (isVisibleFromLeft ns) (reverse . isVisibleFromLeft . reverse $ ns)

arrOr :: Array r Ix2 Bool -> Array r Ix2 Bool -> Array r Ix2 Bool
arrOr = A.zipWith (||)

visibles :: Forest r -> Array r Ix2 Bool
visibles forest = hVisibles `arrOr` vVisibles where
  hVisibles = leftVisibles `arrOr` rightVisibles
  vVisibles = topVisibles `arrOr` bottomVisibles

  (height :. width) = A.size forest

  leftVisibles = 


-- allVisible ∷ Forest r → [[Bool]]
-- allVisible forest = zip2dWith (||) hVisibles vVisibles
--   where
--     hVisibles = map isVisibleFromEitherSide forest
--     vVisibles = transpose . map isVisibleFromEitherSide . transpose $ forest
-- 
--     zip2dWith ∷ (a → b → c) → [[a]] → [[b]] → [[c]]
--     zip2dWith f = zipWith (zipWith f)

--
-- Part 2
--

-- part2 ∷ Forest U → Int
-- part2 forest = maximum . map score $ [0..height-1] `zipA` [0..width-1]
--   where
--     height = Vec.length forest
--     width = Vec.length (forest ! 0)

--     getTree (i', j') = forest ! i' ! j'

--     score ∷ (Int, Int) → Int
--     score (i, j) = product . map (viewingDistance . map getTree) $ [up, down, left, right]
--       where
--         heightHere = getTree (i, j)

--         up    = map (,j) [i-1, i-2 .. 0]
--         down  = map (,j) [i+1, i+2 .. height-1]
--         left  = map (i,) [j-1, j-2 .. 0]
--         right = map (i,) [j+1, j+2 .. width-1]

--         viewingDistance ∷ [Int] → Int
--         viewingDistance = \case
--           [] -> 0
--           (n:ns) | n >= heightHere -> 1
--                  | otherwise -> 1 + viewingDistance ns

main ∷ IO ()
main = do
  let forest = parseInput exampleInput
  print forest
  -- aocSinglePartMain "inputs/08.txt" exampleInput parseInput part1
  -- aocMain "inputs/08.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

exampleInput :: Text
exampleInput = toText @String [str|30373
25512
65332
33549
35390|]
