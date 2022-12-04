module AOC where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Solution i o1 o2 = Solution {
  parse ∷ Text → i,
  solvePart1 ∷ i → o1,
  solvePart2 ∷ i → o2
}

aocMain ∷ (Show o1, Show o2) ⇒ FilePath → Solution i o1 o2 → IO ()
aocMain inputPath Solution {..} = do
  input <- parse <$> T.readFile inputPath
  putStrLn $ "part 1: " <> show (solvePart1 input)
  putStrLn $ "part 2: " <> show (solvePart2 input)
