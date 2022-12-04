module Day01 where

import Utils (tRead)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromJust)
import AOC


solution = Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()


main = aocMain "inputs/01.txt" solution
