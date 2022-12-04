module Day14 where

import Utils (tReadMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromJust)
import AOC


main = aocMain "inputs/14.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
