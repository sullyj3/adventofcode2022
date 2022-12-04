{-# LANGUAGE RecordWildCards #-}
module Day07 where

import Utils (tReadMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Maybe (fromJust)
import AOC


solution = Solution {..}
  where
    parse = id
    solvePart1 = const ()
    solvePart2 = const ()


main = aocMain "inputs/07.txt" solution
