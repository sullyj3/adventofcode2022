module Day04 (main) where

import           Utils       (count)

import           AOC
import           AOC.Parse
import           AOC.Parsers (linesOf, numPair, pairOf)

type ElfPair = (Range, Range)
type Range = (Int, Int)

oneContainsOther ∷ Range → Range → Bool
oneContainsOther a b = (a `rangeContains` b) || (b `rangeContains` a)

rangeContains ∷ Range → Range → Bool
rangeContains (a, b) (c, d) = a <= c && b >= d

overlaps ∷ Range → Range → Bool
overlaps (a,b) (c, d) = (a<=c || a<=d) -- a not greater than second range, and
                     && (c<=a || c<=b) -- c not greater than first range

main ∷ IO ()
main = aocMain "inputs/day04.txt" Solution {..}
  where
    parse ∷ Text → [ElfPair]
    parse = unsafeParse $ linesOf $ pairOf (numPair "-") ","

    solvePart1 ∷ [ElfPair] → Int
    solvePart1 = count $ uncurry oneContainsOther

    solvePart2 ∷ [ElfPair] → Int
    solvePart2 = count $ uncurry overlaps
