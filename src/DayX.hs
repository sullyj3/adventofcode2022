module DayX where

import           AOC
import qualified Data.Text as T
import           Utils     (tRead)


main ∷ IO ()
main = aocMain "inputs/X.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
