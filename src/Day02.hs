{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Day02 where

import Data.Semigroup (Endo(..))
import Optics.Core

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show, Ord, Enum)

data Result = Loss | Draw | Win
  deriving (Eq, Show, Ord, Enum)

parseResult c = case c of
  'X' -> Loss
  'Y' -> Draw
  'Z' -> Win

parseRPS c = case c of
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors

solvePart1 :: String -> Int
solvePart1 = sum . map (uncurry scorePart1) . parsePart1
  where
    parsePart1Line [them, _, us] = (parseRPS them, parseRPS us)
    parsePart1 = map parsePart1Line . lines
    scorePart1 them us = resultScore (result us them) + shapeScore us
    
solvePart2 = sum . map (uncurry scorePart2) . parsePart2
  where
    parsePart2Line [them, _, desiredResult] = (parseRPS them, parseResult desiredResult)
    parsePart2 = map parsePart2Line . lines

    scorePart2 them desiredResult = shapeScore ourPlay + resultScore desiredResult
      where   
        ourPlay = reverseEngineerPlay them desiredResult

reverseEngineerPlay :: RPS -> Result -> RPS
reverseEngineerPlay them desiredResult = case desiredResult of
  Draw -> them
  Win -> beat them
  Loss -> lose them
  where 
    enumIso :: Enum s => Iso' Int s
    enumIso = iso toEnum fromEnum

    -- the data type is defined in order Rock, Paper, Scissors
    -- if we consider the cycle of those constructors, each one
    -- beats its predecessor
    enumly = under enumIso
    beat = enumly $ (`mod` 3) . succ
    lose = enumly $ (`mod` 3) . pred

  
result :: RPS -> RPS -> Result
result us them = toEnum $ (fromEnum us - fromEnum them + 1) `mod` 3

shapeScore :: RPS -> Int
shapeScore us = fromEnum us + 1

resultScore :: Result -> Int
resultScore theResult = 3 * fromEnum theResult

main = do
  contents <- readFile "inputs/day02.txt"
  print $ solvePart1 contents
  print $ solvePart2 contents
