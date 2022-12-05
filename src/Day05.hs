{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day05 where

import           AOC
import           AOC.Parse     hiding (State)
import           Control.Arrow ((***))
import           Data.Char     (isDigit, isSpace)
import qualified Data.Text     as T
import           Prelude       hiding (some)
import qualified Relude.Unsafe as Unsafe
import           Relude.Unsafe ((!!))
import           Utils         (selectIndices)

-------------
-- Parsing --
-------------

splitCratesInstructions ∷ [Text] → ([Text], [Text])
splitCratesInstructions input = (crates, instructions)
  where
    (crates, _numRow : _blankLine : instructions) = break isNumRow input
    -- phoenix combinator!
    isNumRow = T.all $ liftA2 (||) isSpace isDigit

type CrateStack = [Char]

-- given lines of input, return a list of crate columns
parseCrates ∷ [Text] → [CrateStack]
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

data Instruction = Instruction { insCount ∷ Int
                               , insFrom  ∷ Int
                               , insTo    ∷ Int }
  deriving Show

parseInstruction ∷ Parser Instruction
parseInstruction = nonDigits *> do
  [a, b, c] <- decimal `sepEndBy` nonDigits
  -- we subtract 1 from all indices so that we can use 0 based indexing with !!
  pure $ Instruction { insCount=a, insFrom=b-1, insTo=c-1 }
  where
    nonDigits = some (satisfy $ not . isDigit)

parseInstructions ∷ [Text] → [Instruction]
parseInstructions = map (unsafeParse parseInstruction)

parseDay05 ∷ Text → ([CrateStack], [Instruction])
parseDay05 = (parseCrates *** parseInstructions) . splitCratesInstructions . lines

---------------
-- Solutions --
---------------

modifyNth ∷ Int → (a → a) → [a] → [a]
modifyNth _ _ []     = []
modifyNth 0 f (x:xs) = f x : xs
modifyNth n f (x:xs) = x : modifyNth (n-1) f xs

performInstruction ∷ (CrateStack → CrateStack) → Instruction → State [CrateStack] ()
performInstruction pickUp (Instruction {..}) = do
  chosen <- pickUp . take insCount . (!! insFrom) <$> get
  modify $ (insFrom `modifyNth` drop insCount)
         . (insTo `modifyNth` (chosen++))

finalTopCrates
  ∷ (Instruction → State [CrateStack] ()) → ([CrateStack], [Instruction]) → String
finalTopCrates perform (initialStacks, instructions) = map Unsafe.head finalStacks
  where finalStacks = flip execState initialStacks $ traverse perform instructions

main ∷ IO ()
main = aocMain "inputs/05.txt" Solution {..}
  where
    parse = parseDay05

    solvePart1 ∷ ([CrateStack], [Instruction]) → String
    solvePart1 = finalTopCrates $ performInstruction reverse

    solvePart2 ∷ ([CrateStack], [Instruction]) → String
    solvePart2 = finalTopCrates $ performInstruction id
