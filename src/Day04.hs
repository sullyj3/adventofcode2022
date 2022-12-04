module Day04 where

import Utils (tRead, count, both, twoListToPair)
import qualified Data.Text as T
import Data.Text (Text)

import AOC

type ElfPair = (Range, Range)
type Range = (Int, Int)

parseRange ∷ Text → Range
parseRange = both tRead . twoListToPair . T.splitOn "-" 

parseElfPair ∷ Text → ElfPair
parseElfPair = both parseRange . twoListToPair . T.splitOn ","

oneContainsOther ∷ Range → Range → Bool
oneContainsOther a b = (a `rangeContains` b) || (b `rangeContains` a)

rangeContains ∷ Range → Range → Bool
rangeContains (a, b) (c, d) = a <= c && b >= d

overlaps ∷ Range → Range → Bool
overlaps (a,b) (c, d) = not $ (a<c && b<c) || (a>c && a>d)


main = aocMain "inputs/day04.txt" Solution {..}
  where
    parse ∷ Text → [ElfPair]
    parse = map parseElfPair . T.lines

    solvePart1 ∷ [ElfPair] → Int
    solvePart1 = count $ uncurry oneContainsOther

    solvePart2 ∷ [ElfPair] → Int
    solvePart2 = count $ uncurry overlaps
