module Day06 (main) where

import           AOC
import Data.List (nub)

-------------
-- Parsing --
-------------
parseInput :: Text -> String
parseInput = toString

---------------
-- Solutions --
---------------

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = takeWhile ((== n) . length) . map (take n) . tails

allDistinct :: Eq a => [a] -> Bool
allDistinct xs = nub xs == xs

findIndex :: (a -> Bool) -> [a] -> Int
findIndex p = go 0
  where
    go _ [] = error "No elements match the predicate"
    go i (x:xs) = if p x then i else go (i + 1) xs

part1 :: String -> Int
part1 = (4+) . findIndex allDistinct . slidingWindow 4

part2 :: String -> Int
part2 = (14+) . findIndex allDistinct . slidingWindow 14

main ∷ IO ()
main = do 
  -- other testing here

  aocMain "inputs/06.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

