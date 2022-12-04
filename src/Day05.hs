module Day05 where

import Utils (tReadMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Maybe (fromJust)


parse = id


solvePart1 = const ()


solvePart2 = const ()


main = do
  input <- parse <$> T.readFile "inputs/05.txt"
  putStrLn $ "part 1: " <> show (solvePart1 input)
  putStrLn $ "part 2: " <> show (solvePart2 input)
