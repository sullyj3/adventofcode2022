module Day23 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import qualified Data.Text   as T
import           Utils       (tRead)

main ∷ IO ()
main = aocMain "inputs/23.txt" Solution {..}
  where
    parse = unsafeParse $ undefined

    solvePart1 = const ()

    solvePart2 = const ()
