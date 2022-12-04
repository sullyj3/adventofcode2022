{-# LANGUAGE RecordWildCards #-}
module Day12 where

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


main = aocMain "inputs/12.txt" solution
