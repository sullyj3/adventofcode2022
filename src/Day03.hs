module Day03 (main) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (intersect, foldl1')
import AOC
import qualified Data.Text as T
import qualified Relude.Unsafe as Unsafe
import Utils (unreachable)

chunksOf ∷ Int → [a] → [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n rest 
  where (chunk, rest) = splitAt n xs


priority ∷ Char → Int
priority c
  | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
  | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = unreachable


main ∷ IO ()
main = aocMain "inputs/day03.txt" Solution {..}
  where
    parse ∷ Text → [Text]
    parse = lines

    solvePart1 ∷ [Text] → Int
    solvePart1 = sum . map (priority . findDup)
      where
        findDup ∷ Text → Char
        findDup s = Unsafe.head . uncurry (intersect `on` toString) . T.splitAt (T.length s `div` 2) $ s

    solvePart2 ∷ [Text] → Int
    solvePart2 = sum . map (priority . findCommon) . chunksOf 3
      where
        findCommon :: [Text] -> Char
        findCommon = Unsafe.head . foldl1' intersect . map toString
