module Day09 (main) where

import           AOC
import           AOC.Parse            hiding (State)
import           AOC.Parsers
import qualified Data.Set             as Set
import           Data.Strict          (Pair ((:!:)))
import qualified Data.Strict          as Strict
import qualified Relude.Unsafe        as Unsafe
import           Text.Megaparsec.Char (upperChar)
import           Utils                (CardinalDir (..), (.:))
import           V2Lite

type Coord = V2 Int
type VisitedSet = Set Coord
type Rope = [Coord]
type Instruction = (CardinalDir, Int)

-------------
-- Parsing --
-------------
parseInput ∷ Text → [Instruction]
parseInput = unsafeParse $ linesOf $ pairOfBoth direction decimal " "
  where
    direction ∷ Parser CardinalDir
    direction = upperChar >>= \case
      'U' -> pure U
      'D' -> pure D
      'L' -> pure L
      'R' -> pure R
      c   -> fail $ "Invalid direction: " <> [c]

---------------
-- Solutions --
---------------
updateChild ∷ Coord → Coord → Coord
updateChild newParent oldChild
  | touching = oldChild
  | otherwise = oldChild + signum (newParent - oldChild)
  where
    touching = newParent `chebyshevDist` oldChild <= 1

-- Given a new location of the head, updates the rope, returning the new
-- location of the tail
updateRope ∷ Coord → State Rope Coord
updateRope = state . curry \case
  (_______, []    ) -> error "updateRope: empty rope"
  (newHead, _:rope) -> (newTail, newHead:rope')
    where
      (newTail, rope') = mapAccumL (join (,) .: updateChild) newHead rope

executeInstruction ∷ Pair Rope VisitedSet → Instruction → Pair Rope VisitedSet
executeInstruction (rope :!: visited) (dir, steps) =
  rope' :!: Set.fromList tailLocations <> visited
  where
    oldHead = Unsafe.head rope
    instructionOffsets = take steps . drop 1 $ iterate (move1Cardinal dir) oldHead
    (tailLocations, rope') = runState (traverse updateRope instructionOffsets) rope

tailVisitCount ∷ Int → [Instruction] → Int
tailVisitCount ropeLength = Set.size
  . Strict.snd
  . foldl' executeInstruction (replicate ropeLength 0 :!: mempty)

part1 ∷ [Instruction] → Int
part1 = tailVisitCount 2

part2 ∷ [Instruction] → Int
part2 = tailVisitCount 10

main ∷ IO ()
main = do
  aocMain "inputs/09.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
