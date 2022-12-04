module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

import Data.Time.Clock
import Data.Time (toGregorian)


main :: IO ()
main = do 
  currentTime <- getCurrentTime
  let (_, month, day) = toGregorian (utctDay currentTime)
  if month == 12 && day <= 25 then
    putStrLn $ "It's currently day " ++ show day
  else 
    putStrLn "It's not currently advent. You'll need to specify a day to run"

  putStrLn $ "Running day " ++ show day
  dayMains !! (day - 1)

dayMains = 
  [ Day01.main
  , Day02.main
  , Day03.main
  , Day04.main
  , Day05.main
  , Day06.main
  , Day07.main
  , Day08.main
  , Day09.main
  , Day10.main
  , Day11.main
  , Day12.main
  , Day13.main
  , Day14.main
  , Day15.main
  , Day16.main
  , Day17.main
  , Day18.main
  , Day19.main
  , Day20.main
  , Day21.main
  , Day22.main
  , Day23.main
  , Day24.main
  , Day25.main
  ]
