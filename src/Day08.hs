{-# LANGUAGE TupleSections #-}
module Day08 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import           Data.Char            (digitToInt)
import           Data.List            (maximum)
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as Vec
import           Prelude              hiding (some)
import           Text.Megaparsec.Char (digitChar)
import           Utils                (count, mapWithState)

type Forest = [[Int]]


-------------
-- Parsing --
-------------
parseInput ∷ Text → Forest
parseInput = unsafeParse (linesOf (some $ digitToInt <$> digitChar))

---------------
-- Solutions --
---------------

isVisibleFromLeft ∷ [Int] -> [Bool]
isVisibleFromLeft = mapWithState (\n currMax -> (currMax < n, max n currMax)) minBound

isVisibleFromEitherSide ∷ [Int] → [Bool]
isVisibleFromEitherSide ns = zipWith (||)
  (isVisibleFromLeft ns) (reverse . isVisibleFromLeft . reverse $ ns)


allVisible ∷ Forest → [[Bool]]
allVisible forest = zip2dWith (||) hVisibles vVisibles
  where
    hVisibles = map isVisibleFromEitherSide forest
    vVisibles = transpose . map isVisibleFromEitherSide . transpose $ forest

    zip2dWith ∷ (a → b → c) → [[a]] → [[b]] → [[c]]
    zip2dWith f = zipWith (zipWith f)

viewingDistance ∷ Int → [Int] → Int
viewingDistance currHeight = go
  where
    go [] = 0
    go (n:ns)
      | n >= currHeight = 1
      | otherwise = 1 + go ns

--
-- Part 1
--
part1 ∷ Forest → Int
part1 = sum . map (count id) . allVisible

--
-- Part 2
--
score ∷ (Int, Int) → Vector (Vector Int) → Int
score (i, j) forest =
  product . map (viewingDistance heightHere . map getTree) $ [up, down, left, right]
  where
    heightHere = getTree (i, j)
    getTree (i', j') = forest ! i' ! j'

    up = map (, j) [i-1,i-2..0]
    down = map (, j) [i+1,i+2..height - 1]
    left = map (i, ) [j-1,j-2..0]
    right = map (i, ) [j+1,j+2..width - 1]

    height = Vec.length forest
    width = Vec.length (forest ! 0)

part2 ∷ Forest → Int
part2 (list2vec2d -> forest) = maximum do
  i <- [0..Vec.length forest - 1]
  j <- [0..Vec.length forest - 1]
  pure $ score (i, j) forest

list2vec2d ∷ [[a]] → Vector (Vector a)
list2vec2d = Vec.fromList . map Vec.fromList

main ∷ IO ()
main = do
  aocMain "inputs/08.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
