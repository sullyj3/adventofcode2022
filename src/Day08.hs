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

--
-- Part 1
--
part1 ∷ Forest → Int
part1 = sum . map (count id) . allVisible

isVisibleFromLeft ∷ [Int] → [Bool]
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

--
-- Part 2
--

part2 ∷ Forest → Int
part2 (list2vec2d -> forest) =
  maximum . map score $ liftA2 (,) [0..height-1] [0..width-1]
  where
    height = Vec.length forest
    width = Vec.length (forest ! 0)

    getTree (i', j') = forest ! i' ! j'

    score ∷ (Int, Int) → Int
    score (i, j) = product . map (viewingDistance . map getTree) $ [up, down, left, right]
      where
        heightHere = getTree (i, j)

        up    = map (,j) [i-1, i-2 .. 0]
        down  = map (,j) [i+1, i+2 .. height-1]
        left  = map (i,) [j-1, j-2 .. 0]
        right = map (i,) [j+1, j+2 .. width-1]

        viewingDistance ∷ [Int] → Int
        viewingDistance = \case
          [] -> 0
          (n:ns) | n >= heightHere -> 1
                 | otherwise -> 1 + viewingDistance ns

list2vec2d ∷ [[a]] → Vector (Vector a)
list2vec2d = Vec.fromList . map Vec.fromList

main ∷ IO ()
main = do
  aocMain "inputs/08.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
