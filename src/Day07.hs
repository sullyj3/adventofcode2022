module Day07 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import qualified Data.Text   as T
import           Utils       (tRead)

-------------
-- Parsing --
-------------
parseInput = id

---------------
-- Solutions --
---------------
part1 = const ()
part2 = const ()

main ∷ IO ()
main = do 
  -- other testing here

  aocMain "inputs/07.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

