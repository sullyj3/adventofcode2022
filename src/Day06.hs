module Day06 (main) where

import           AOC
import           Data.List     (findIndex)
import           Relude.Unsafe (fromJust)
import Utils (slidingWindow, allDistinct)

-------------
-- Parsing --
-------------
parseInput ∷ Text → String
parseInput = toString

---------------
-- Solutions --
---------------
part1 ∷ String → Int
part1 = (4+) . fromJust . findIndex allDistinct . slidingWindow 4

part2 ∷ String → Int
part2 = (14+) . fromJust . findIndex allDistinct . slidingWindow 14

main ∷ IO ()
main = do
  aocMain "inputs/06.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

