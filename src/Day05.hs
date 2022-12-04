module Day05 where

import           AOC
import qualified Data.Text as T
import           Utils     (tRead)


main ∷ IO ()
main = aocMain "inputs/05.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
