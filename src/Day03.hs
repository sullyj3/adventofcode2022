module Day03 (main) where

import           AOC
import           Data.Char       (isAsciiLower, isAsciiUpper)
import           Data.List       (foldl1', intersect)
import           Data.List.Extra (chunksOf)
import qualified Data.Text       as T
import qualified Relude.Unsafe   as Unsafe
import           Utils           (unreachable)

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

    totalPriority ∷ [Char] → Int
    totalPriority = sum . map priority

    part1 ∷ [Text] → Int
    part1 = totalPriority . map findDup
      where
        findDup ∷ Text → Char
        findDup = Unsafe.head . uncurry (intersect `on` toString) . halves

        halves s = T.splitAt (T.length s `div` 2) s

    part2 ∷ [Text] → Int
    part2 = totalPriority . map findCommon . chunksOf 3
      where
        findCommon ∷ [Text] → Char
        findCommon = Unsafe.head . foldl1' intersect . map toString

