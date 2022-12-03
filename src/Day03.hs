module Day03 where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (intersect, nub, foldl1')


solvePart1 :: String -> Int
solvePart1 = sum . map (priority . findDup) . lines
  where
    findDup s = let
      (left, right) = splitAt (length s `div` 2) s
      [dup] = nub $ left `intersect` right
      in dup


chunksOf n [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs
                 in chunk : chunksOf n rest


solvePart2 = sum . map (priority . findCommon) . chunksOf 3 . lines
  where
    findCommon = head . nub . foldl1' intersect


priority :: Char -> Int
priority c
  | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
  | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = undefined


main = do
  contents <- readFile "inputs/day03.txt"
  print $ solvePart1 contents
  print $ solvePart2 contents
  pure ()
