{-# LANGUAGE OverloadedStrings #-}
module Day04 where

import Utils (tReadMaybe, count)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Maybe (fromJust)

type ElfPair = (Range, Range)
type Range = (Int, Int)

twoListToPair [a,b] = (a,b)

parseRange :: Text -> (Int, Int)
parseRange = twoListToPair . fromJust . traverse tReadMaybe . T.splitOn "-" 

parseElfPair :: Text -> ElfPair
parseElfPair = twoListToPair . map parseRange . T.splitOn ","

parse :: Text -> [ElfPair]
parse = map parseElfPair . T.lines

oneContainsOther :: Range -> Range -> Bool
oneContainsOther a b = (a `rangeContains` b) || (b `rangeContains` a)

rangeContains :: Range -> Range -> Bool
rangeContains (a, b) (c, d) = a <= c && b >= d

solvePart1 :: [ElfPair] -> Int
solvePart1 = count $ uncurry oneContainsOther

solvePart2 :: [ElfPair] -> Int
solvePart2 = count $ uncurry overlaps

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

overlaps :: Range -> Range -> Bool
overlaps (a,b) (c, d) = not $ (a<c && b<c) || (a>c && a>d)

testData = [
  ((1,2), (2,4)),
  ((5,6), (6,7)),
  ((1,2), (6,7))
  ]

main = do
  pairs <- parse <$> T.readFile "inputs/day04.txt"
  putStrLn $ "part 1: " <> show (solvePart1 pairs)
  putStrLn $ "part 2: " <> show (solvePart2 pairs)
