module Day05 where

import           AOC
import           AOC.Parse              hiding (State)
import           AOC.Parsers            (linesOf, numLine)
import           Control.Arrow          ((***))
import           Data.Char              (isUpper)
import qualified Data.Text              as T
import           Optics.At.Core         (ix)
import           Optics.State.Operators ((%=))
import           Prelude                hiding (some)
import qualified Relude.Unsafe          as Unsafe
import           Relude.Unsafe          ((!!))
import           Utils                  (selectIndices)

type CrateStack = [Char]

data Instruction = Instruction { count ∷ Int
                               , from  ∷ Int
                               , to    ∷ Int }
  deriving Show

-------------
-- Parsing --
-------------

-- return a list of crate columns
parseCrates ∷ Text → [CrateStack]
parseCrates =
    map (toString . T.filter isUpper)
  . selectIndices [1,5..]
  . T.transpose
  . lines

parseInstructions ∷ Text → [Instruction]
parseInstructions = unsafeParse $ linesOf (toInstruction <$> numLine)
  where
    -- subtract 1 from indices because haskell uses 0-based indexing, not 1-based
    toInstruction [a,b,c] = Instruction { count=a, from=b-1, to=c-1 }
    toInstruction _ = error "line does not contain exactly 3 numbers"

parseDay05 ∷ Text → ([CrateStack], [Instruction])
parseDay05 = (parseCrates *** parseInstructions) . splitCratesInstructions
  where
    splitCratesInstructions ∷ Text → (Text, Text)
    splitCratesInstructions = second (T.drop 2) . T.breakOn "\n\n"

---------------
-- Solutions --
---------------

performInstruction ∷ (CrateStack → CrateStack) → Instruction → State [CrateStack] ()
performInstruction possiblyReverse instruct = do
  movedCrates <- possiblyReverse . take instruct.count . (!! instruct.from) <$> get
  ix instruct.from %= drop instruct.count
  ix instruct.to   %= (movedCrates ++)

finalTopCrates
  ∷ (CrateStack → CrateStack) → ([CrateStack], [Instruction]) → String
finalTopCrates possiblyReverse (initialStacks, instructions) = map Unsafe.head finalStacks
  where
    finalStacks = execState performInstructions initialStacks
    performInstructions = traverse (performInstruction possiblyReverse) instructions

main ∷ IO ()
main = aocMain "inputs/05.txt" Solution {..}
  where
    parse = parseDay05

    part1 ∷ ([CrateStack], [Instruction]) → String
    part1 = finalTopCrates reverse

    part2 ∷ ([CrateStack], [Instruction]) → String
    part2 = finalTopCrates id
