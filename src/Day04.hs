module Day04 where

import Utils (parseBinary)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)


solvePart1 = const ()


solvePart2 = const ()


main = do
  contents <- T.lines <$> T.readFile "inputs/day04.txt"
  print $ solvePart1 contents
  print $ solvePart2 contents
  pure ()
