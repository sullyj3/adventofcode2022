module Day01 where

import           AOC
import           Data.Maybe (fromJust)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Utils      (tRead)


solution = Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()


main = aocMain "inputs/01.txt" solution
