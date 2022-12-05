module Day11 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import qualified Data.Text   as T
import           Utils       (tRead)

main ∷ IO ()
main = aocMain "inputs/11.txt" Solution {..}
  where
    parse = unsafeParse $ undefined

    solvePart1 = const ()

    solvePart2 = const ()
