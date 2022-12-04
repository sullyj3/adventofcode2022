{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module Day04 where

import Utils (tReadMaybe, count)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Maybe (fromJust)

import AOC

type ElfPair = (Range, Range)
type Range = (Int, Int)

twoListToPair [a,b] = (a,b)

parseRange ∷ Text → (Int, Int)
parseRange = twoListToPair . fromJust . traverse tReadMaybe . T.splitOn "-" 

parseElfPair ∷ Text → ElfPair
parseElfPair = twoListToPair . map parseRange . T.splitOn ","

oneContainsOther ∷ Range → Range → Bool
oneContainsOther a b = (a `rangeContains` b) || (b `rangeContains` a)

rangeContains ∷ Range → Range → Bool
rangeContains (a, b) (c, d) = a <= c && b >= d

-- | ... |
--    | ... |
--
--    | ... |
--  |      ... |
--
--    | ... |
--     | . |
--
--        | ... |
--    | ... |

overlaps ∷ Range → Range → Bool
overlaps (a,b) (c, d) = not $ (a<c && b<c) || (a>c && a>d)


solution = Solution {..}
  where
    parse ∷ Text → [ElfPair]
    parse = map parseElfPair . T.lines

    solvePart1 ∷ [ElfPair] → Int
    solvePart1 = count $ uncurry oneContainsOther

    solvePart2 ∷ [ElfPair] → Int
    solvePart2 = count $ uncurry overlaps

    
main = aocMain "inputs/day04.txt" solution
