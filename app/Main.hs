module Main (main) where

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

import           Control.Exception
import           Data.Time          (localDay, toGregorian, utcToLocalZonedTime,
                                     zonedTimeToLocalTime)
import           Data.Time.Clock

import           Data.Time.Calendar (DayOfMonth, MonthOfYear, Year)
import qualified Relude.Unsafe      as Unsafe

data Invocation = CurrentDay | GivenDay Int | AllDays


parseArgs ∷ [String] → Invocation
parseArgs = \case
  []      → CurrentDay
  ["all"] → AllDays
  [n]     → GivenDay $ Unsafe.read n
  _       → error "too many arguments!"


main ∷ IO ()
main = do
  invocation ← parseArgs <$> getArgs
  case invocation of
    CurrentDay → currentDay
    GivenDay n → runDay n
    -- TODO only run days that have happened yet
    AllDays    → traverse_ runDay [1..25]


currentDay ∷ IO ()
currentDay = do
  date@(_year, month, day) ← getDateGregorian
  putStrLn $ "Today is " ++ formatDateAustralian date
  handleToday day month
  where
    isAdvent day month = month == 12 && day <= 25

    handleToday day month
      | isAdvent day month = do
        putStrLn $ "It's day " ++ show day ++ " of Advent!"
        runDay day
      | otherwise = putStrLn "It's not currently Advent. You'll need to specify a day to run."

    getDateGregorian ∷ IO (Year, MonthOfYear, DayOfMonth)
    getDateGregorian = do
      currentTime ← getCurrentTime
      localTime ← utcToLocalZonedTime currentTime
      let localTime' = zonedTimeToLocalTime localTime
      pure $ toGregorian $ localDay localTime'

    formatDateAustralian ∷ (Year, MonthOfYear, DayOfMonth) → String
    formatDateAustralian (year, month, day) =
      show day ++ "/" ++ show month ++ "/" ++ show year


-- Handles errors without crashing
runDay ∷ Int → IO ()
runDay day = do
  putStrLn "-------------------------------"
  putStrLn $ "Running day " ++ show day
  result ← try (dayMains Unsafe.!! (day - 1)) ∷ IO (Either SomeException ())
  case result of
    Right () → pure ()
    Left err → putStrLn $ "Day " <> show day <> " failed with error: " <> show err


dayMains ∷ [IO ()]
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
