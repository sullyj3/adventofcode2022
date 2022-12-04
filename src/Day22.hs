module Day22 (main) where

import Utils (tRead)
import qualified Data.Text as T
import AOC


main :: IO ()
main = aocMain "inputs/22.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
