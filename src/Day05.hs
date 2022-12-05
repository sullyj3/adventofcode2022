{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day05 where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import           Data.Char   (isDigit, isSpace)
import qualified Data.Text   as T
import           Utils       (selectIndices, tRead)
import Text.Megaparsec.Char (string, space)
import Relude.Unsafe ((!!))
import qualified Relude.Unsafe as Unsafe

{-
 -
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3
 1   5   9   13
 4n+1
 -}

splitCratesInstructions ∷ [Text] → ([Text], [Text])
splitCratesInstructions input = (crates, instructions)
  where
    (crates, _numRow : _blankLine : instructions) = break isNumRow input
    isNumRow = T.all isDigit . T.filter (not . isSpace)

-- return a list of crate columns
parseCrates ∷ [Text] → [[Char]]
parseCrates =
    map catMaybes
  . transpose
  . map parseCrateRow

parseCrateRow ∷ Text → [Maybe Char]
parseCrateRow = map keepAlphas
              . selectIndices [1,5..]
              . toString
  where
    keepAlphas c | isSpace c = Nothing
                 | otherwise = Just c

data Instruction = Instruction { insCount :: Int
                               , insFrom  :: Int
                               , insTo    :: Int }
  deriving Show

parseInstruction :: Parser Instruction
parseInstruction = liftA3 Instruction
  (string "move " *> decimal <* space)
  -- we subtract 1 from all indices so that we can use 0 based indexing with !!
  (string "from " *> (pred <$> decimal) <* space)
  (string "to "   *> (pred <$> decimal) <* space)

parseInstructions :: [Text] -> [Instruction]
parseInstructions = map (unsafeParse parseInstruction) 

type CrateStack = [Char]

parseDay05 :: Text -> ([CrateStack], [Instruction])
parseDay05 input = (parseCrates crateLines, parseInstructions instructionLines)
  where
    (crateLines, instructionLines) = splitCratesInstructions . lines $ input

test ∷ IO ()
test = do
  -- input <- readFileText "inputs/05.txt"
  -- let (crates, instructions) = splitCratesInstructions . lines $ input
  -- print instructions
  -- let (parsedCrates, parsedInstructions) = parseDay05 input
  let testCrateStacks = ["DNZ", "CM", "P"]

  putText "Before:"
  print testCrateStacks
  putText "applying test instruction:"
  let testInstruction = Instruction 3 0 2
  print testInstruction
  putText "After:"
  print $ performInstruction testInstruction testCrateStacks

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth 0 f (x:xs) = f x : xs
modifyNth n f (x:xs) = x : modifyNth (n-1) f xs


performInstruction :: Instruction -> [CrateStack] -> [CrateStack]
performInstruction Instruction {..} stacks = stacks'
  where chosen = take insCount (stacks !! insFrom)
        stacks' = (insFrom `modifyNth` drop insCount)
                . (insTo `modifyNth` (reverse chosen ++))
                $ stacks

main ∷ IO ()
main = aocMain "inputs/05.txt" Solution {..}
  where
    parse = parseDay05

    solvePart1 :: ([CrateStack], [Instruction]) -> String
    solvePart1 (initialStacks, instructions) = map Unsafe.head finalStacks
      where finalStacks = foldl' (flip performInstruction) initialStacks instructions

    solvePart2 = const ()
