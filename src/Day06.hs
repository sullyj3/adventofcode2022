module Day06 where

import           AOC
import qualified Data.Text as T
import           Utils     (tRead)


main ∷ IO ()
main = aocMain "inputs/06.txt" Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()
