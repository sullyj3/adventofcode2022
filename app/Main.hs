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

import Control.Exception
import Data.Time.Clock
import Data.Time (toGregorian, localDay, zonedTimeToLocalTime)
import System.Environment (getArgs)
import Data.Foldable (traverse_)
import Data.Time.LocalTime (utcToLocalTime)
import Data.Time (utcToLocalZonedTime)


data Invocation = CurrentDay | GivenDay Int | AllDays


parseArgs ∷ [String] → Invocation
parseArgs = \case
  [] → CurrentDay
  ["all"] → AllDays
  [n] → GivenDay $ read n
  _ → error "too many arguments!"


main ∷ IO ()
main = do 
  invocation ← parseArgs <$> getArgs
  case invocation of
    CurrentDay → currentDay
    GivenDay n → runDay n
    AllDays → traverse_ runDay [1..25]


currentDay ∷ IO ()
currentDay = do
  currentTime <- getCurrentTime
  localTime <- utcToLocalZonedTime currentTime
  let localTime' = zonedTimeToLocalTime localTime
      (year, month, day) = toGregorian $ localDay localTime'
  putStrLn $ "Today is " ++ show day ++ "/" ++ show month ++ "/" ++ show year
  handleToday day month
  where
    isAdvent day month = month == 12 && day <= 25

    handleToday day month
      | isAdvent day month = do
        putStrLn $ "It's day " ++ show day ++ " of Advent!"
        runDay day
      | otherwise = putStrLn "It's not currently Advent. You'll need to specify a day to run."


-- Handles errors without crashing
runDay ∷ Int → IO ()
runDay day = do
  putStrLn "-------------------------------"
  putStrLn $ "Running day " ++ show day
  result <- try (dayMains !! (day - 1)) ∷ IO (Either SomeException ())
  case result of
    Right () → pure ()
    Left err → putStrLn $ "Day " <> show day <> " failed with error: " <> show err


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
