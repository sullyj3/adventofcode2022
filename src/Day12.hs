module Day12 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import qualified Data.Text   as T
import           PyF
import           Utils       (tRead)

-------------
-- Parsing --
-------------
parseInput = id
-- parseInput = unsafeParse numLine
-- parseInput = toString

---------------
-- Solutions --
---------------
part1 = const ()
part2 = const ()

main ∷ IO ()
main = do
  -- other testing here

  aocSinglePartMain "inputs/12.txt" exampleInput parseInput part1

  -- aocMain "inputs/12.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

exampleInput :: Text
exampleInput = toText @String [str||]

