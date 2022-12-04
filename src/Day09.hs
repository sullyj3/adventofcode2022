module Day09 (main) where

import Utils (tRead)
import qualified Data.Text as T
import AOC


main :: IO ()
main = aocMain "inputs/09.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
