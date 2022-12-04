module Day03 where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (intersect, foldl1')
import AOC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function (on)


chunksOf ∷ Int → [a] → [[a]]
chunksOf n [] = []
chunksOf n xs = chunk : chunksOf n rest 
  where (chunk, rest) = splitAt n xs


priority ∷ Char → Int
priority c
  | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
  | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = undefined


main ∷ IO ()
main = aocMain "inputs/day03.txt" Solution {..}
  where
    parse ∷ Text → [Text]
    parse = T.lines

    solvePart1 ∷ [Text] → Int
    solvePart1 = sum . map (priority . findDup)
      where
        findDup ∷ Text → Char
        findDup s = head . uncurry (intersect `on` T.unpack) . T.splitAt (T.length s `div` 2) $ s

    solvePart2 ∷ [Text] → Int
    solvePart2 = sum . map (priority . findCommon) . chunksOf 3
      where
        findCommon :: [Text] -> Char
        findCommon = head . foldl1' intersect . map T.unpack
