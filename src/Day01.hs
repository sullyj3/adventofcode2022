module Day01 where

import           AOC
import           Data.Maybe (fromJust)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Utils      (tRead)


solution = Solution {..}
  where
    parse = id
    part1 = const ()
    part2 = const ()


main = aocMain "inputs/01.txt" solution
