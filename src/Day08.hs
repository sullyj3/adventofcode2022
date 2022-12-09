{-# LANGUAGE TupleSections #-}
module Day08 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import           Data.Char            (digitToInt)
import           Data.Massiv.Array    (Array, B, Comp (..), D, DL,
                                       Dimension (..), Ix2 (..), Lower,
                                       Manifest, Matrix, Source, U, compute)
import qualified Data.Massiv.Array    as A
import           Data.Massiv.Vector   (Vector)
import           Prelude              hiding (some)
import           PyF                  (str)
import           Relude.Unsafe        (fromJust)
import           Text.Megaparsec.Char (digitChar)

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
part1 ∷ Forest U → Int
part1 = A.sum . A.map (\x -> if x then 1 else 0) . visibles

mapOuterSlices ∷ (Source rep1 e, Source rep2 f, A.Index ix, A.Index (Lower ix))
               ⇒ (Array rep1 (Lower ix) e → Array rep2 (Lower ix) f)
               → Array rep1 ix e → Array DL ix f
mapOuterSlices f = fromJust . A.stackOuterSlicesM . A.map f . A.outerSlices

arrScan ∷ (A.Index ix, Source rep e, Manifest rep2 s)
        ⇒ (s → e → s) → s → Array rep ix e → Array rep2 ix s
arrScan f z = flip evalState z . A.traverseA (\x ->
  state $ \s -> let s' = f s x in (s', s'))

-- eg [1,2,2,3] -> [True, True, False, True]
markIncreases ∷ (Ord a, Bounded a, Manifest rep2 Bool, Source rep1 a)
              ⇒ Vector rep1 a
              → Vector rep2 Bool
markIncreases = flip evalState minBound . A.traverseA \x ->
  state \prev -> (prev < x, x)

treesVisibleFromLeft ∷ (Source rep Int) ⇒ Vector rep Int → Vector B Bool
treesVisibleFromLeft treeline = markIncreases scanned
  where
    scanned = arrScan max minBound treeline :: Vector B Int

treesVisibleFromRight ∷ (Source rep Int) ⇒ Vector rep Int → Vector D Bool
treesVisibleFromRight treeline =
  A.reverse Dim1 (treesVisibleFromLeft $ A.reverse Dim1 treeline :: Vector B Bool)

arrOr ∷ (A.Index ix, Source r1 Bool, Source r2 Bool) ⇒ Array r1 ix Bool → Array r2 ix Bool → Array D ix Bool
arrOr = A.zipWith (||)

visibles ∷ (Source r1 Int) ⇒ Forest r1 → Array D Ix2 Bool
visibles forest = hVisibles
  where
  hVisibles ∷ Array D Ix2 Bool
  hVisibles = leftVisibles `arrOr` rightVisibles `arrOr` topVisibles `arrOr` bottomVisibles

  leftVisibles, rightVisibles ∷ Matrix B Bool
  leftVisibles = compute $ mapOuterSlices treesVisibleFromLeft forest
  rightVisibles = compute $ mapOuterSlices treesVisibleFromRight forest

  topVisibles ∷ Matrix B Bool
  topVisibles = compute
    $ A.transpose
    . (compute @B)
    . mapOuterSlices treesVisibleFromLeft
    $ A.transpose forest

  bottomVisibles ∷ Matrix B Bool
  bottomVisibles = compute
    $ A.transpose
    . (compute @B)
    . mapOuterSlices treesVisibleFromRight
    $ A.transpose forest


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
  aocSinglePartMain "inputs/08.txt" exampleInput parseInput part1
  -- aocMain "inputs/08.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

exampleInput ∷ Text
exampleInput = toText @String [str|30373
25512
65332
33549
35390|]
