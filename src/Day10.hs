module Day10 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import qualified Data.Text   as T
import           Utils       (tRead)

main ∷ IO ()
main = aocMain "inputs/10.txt" Solution {..}
  where
    parse = unsafeParse $ undefined

    solvePart1 = const ()

    solvePart2 = const ()
