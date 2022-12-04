module Day05 where

import Utils (tReadMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Maybe (fromJust)
import AOC


main = aocMain "inputs/05.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
