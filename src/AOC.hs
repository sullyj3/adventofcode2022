{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
module AOC where

import qualified Data.Text.IO       as T
import Utils (tShow)

data Solution i o = Solution
  { parse ∷ Text → i
  , part1 ∷ i → o
  , part2 ∷ i → o
  }

aocMain ∷ forall i o. Show o ⇒ FilePath → Solution i o → IO ()
aocMain inputPath sol = do
  parsed <- sol.parse <$> T.readFile inputPath
  putTextLn . unlines $ [ run "part 1: " sol.part1 parsed
                        , run "part 2: " sol.part2 parsed ] 
  where
    run ∷ Text → (i → o) → i → Text
    run msg solve = (msg <>) . tShow . solve
