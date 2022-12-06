module AOC where

import           Utils (tShow)
import System.Clipboard (setClipboardString)
import qualified Data.Text as T

data Solution i o = Solution
  { parse ∷ Text → i
  , part1 ∷ i → o
  , part2 ∷ i → o
  }

-- | prints the answer for both the example input and the real input,
-- and copies the answer to the clipboard
aocSinglePartMain ∷ Show o ⇒ FilePath → Text → (Text → i) → (i → o) → IO ()
aocSinglePartMain inputPath testInput parse solve = do
  solveExample
  solveReal
  where
    solveExample 
      | T.null testInput = do
        putStrLn "No example input provided, skipping"
        pure ()
      | otherwise = do
        putStrLn $ "Example: " <> (show . solve . parse $ testInput)

    solveReal = do
      input <- readFileText inputPath
      let answer = show . solve . parse $ input
      putStrLn $ "Real: " <> answer
      setClipboardString answer
      putStrLn "\nAnswer copied to clipboard!"


-- prints the answer for both part 1 and part 2
-- does not copy the answer to the clipboard
-- best used after the answer has already been submitted and it's time for refinement
aocMain ∷ forall i o. Show o ⇒ FilePath → Solution i o → IO ()
aocMain inputPath sol = do
  parsed <- sol.parse <$> readFileText inputPath
  putTextLn . unlines $ [ run "part 1: " sol.part1 parsed
                        , run "part 2: " sol.part2 parsed ]
  where
    run ∷ Text → (i → o) → i → Text
    run msg solve = (msg <>) . tShow . solve
