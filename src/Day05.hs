{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day05 where

import           AOC
import           AOC.Parse      hiding (State)
import           Control.Arrow  ((***))
import           Data.Char      (isDigit, isUpper)
import qualified Data.Text      as T
import           Optics.At.Core (ix)
import           Prelude        hiding (some)
import qualified Relude.Unsafe  as Unsafe
import           Relude.Unsafe  ((!!))
import           Utils          (selectIndices)
import Optics.State.Operators ((%=))
import AOC.Parsers (linesOf, numLine)

-------------
-- Parsing --
-------------

splitCratesInstructions ∷ Text → (Text, Text)
splitCratesInstructions = second (T.drop 2) . T.breakOn "\n\n"

type CrateStack = [Char]

-- return a list of crate columns
parseCrates ∷ Text → [CrateStack]
parseCrates = 
    map (toString . T.filter isUpper)
  . selectIndices [1,5..]
  . T.transpose
  . lines

data Instruction = Instruction { insCount ∷ Int
                               , insFrom  ∷ Int
                               , insTo    ∷ Int }
  deriving Show

parseInstruction ∷ Parser Instruction
parseInstruction = do
  [a, b, c] <- numLine
  -- we subtract 1 from all indices so that we can use 0 based indexing with !!
  pure $ Instruction { insCount=a, insFrom=b-1, insTo=c-1 }

parseInstructions ∷ Text → [Instruction]
parseInstructions = unsafeParse $ linesOf parseInstruction

parseDay05 ∷ Text → ([CrateStack], [Instruction])
parseDay05 = (parseCrates *** parseInstructions) . splitCratesInstructions

---------------
-- Solutions --
---------------

performInstruction ∷ (CrateStack → CrateStack) → Instruction → State [CrateStack] ()
performInstruction pickUp (Instruction {..}) = do
  chosen <- pickUp . take insCount . (!! insFrom) <$> get
  ix insFrom %= drop insCount
  ix insTo   %= (chosen ++)

finalTopCrates
  ∷ (CrateStack -> CrateStack) → ([CrateStack], [Instruction]) → String
finalTopCrates pickup (initialStacks, instructions) = map Unsafe.head finalStacks
  where 
    finalStacks = execState performInstructions initialStacks
    performInstructions = traverse (performInstruction pickup) instructions

main ∷ IO ()
main = aocMain "inputs/05.txt" Solution {..}
  where
    parse = parseDay05

    solvePart1 ∷ ([CrateStack], [Instruction]) → String
    solvePart1 = finalTopCrates reverse

    solvePart2 ∷ ([CrateStack], [Instruction]) → String
    solvePart2 = finalTopCrates id
