{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module AOC where

import qualified Data.Text.IO as T

data Solution i o = Solution 
  { parse ∷ Text → i
  , part1 ∷ i → o
  , part2 ∷ i → o
  }

aocMain ∷ Show o ⇒ FilePath → Solution i o → IO ()
aocMain inputPath sol = do
  input ← sol.parse <$> T.readFile inputPath
  putStrLn $ "part 1: " <> show (sol.part1 input)
  putStrLn $ "part 2: " <> show (sol.part2 input)
