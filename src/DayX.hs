module DayX where

import Utils (tRead)
import qualified Data.Text as T
import AOC


main :: IO ()
main = aocMain "inputs/X.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
