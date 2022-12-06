module Day06 (main) where

import           AOC
import           Data.List     (findIndex)
import           Relude.Unsafe (fromJust)
import           Utils         (allDistinct, slidingWindow)

findCode ∷ Int → String → Int
findCode n = (n+) . fromJust . findIndex allDistinct . slidingWindow n

main ∷ IO ()
main = do
  aocMain "inputs/06.txt" Solution { parse=toString
                                   , part1=findCode 4
                                   , part2=findCode 14 }

