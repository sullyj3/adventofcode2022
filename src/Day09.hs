module Day09 (main) where

import           AOC
import           AOC.Parse            hiding (State)
import           AOC.Parsers
import qualified Data.Set             as Set
import qualified Relude.Unsafe        as Unsafe
import           Text.Megaparsec.Char (upperChar)
import           Utils                (CardinalDir (..), (.:))
import           V2Lite

type Coord = V2 Int
type VisitedSet = Set Coord
type Rope = [Coord]

-------------
-- Parsing --
-------------
parseInput ∷ Text → [(CardinalDir, Int)]
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

-- returns the new location of the tail
updateRope ∷ Coord → Rope → (Coord, Rope)
updateRope _______ [] = error "updateRope: empty rope"
updateRope newHead (_:rope) = (newTail, newHead:rope')
  where
    (newTail, rope') = mapAccumL (join (,) .: updateChild) newHead rope

executeInstruction ∷ (CardinalDir, Int) → State (Rope, VisitedSet) ()
executeInstruction instruction = do
  (rope, visited) <- get

  let oldHead = Unsafe.head rope
      (tailLocations, rope') = flip runState rope $
        traverse (state . updateRope . (oldHead +))
               $ instructionOffsets instruction

  put (rope', Set.fromList tailLocations <> visited)
  where
    instructionOffsets ∷ (CardinalDir, Int) → [Coord]
    instructionOffsets (dir, steps) = take steps . drop 1 $ iterate (move1Cardinal dir) 0

tailVisitCount ∷ Int → [(CardinalDir, Int)] → Int
tailVisitCount ropeLength instructions = Set.size visited
  where
    (_,visited) = flip execState (replicate ropeLength 0, Set.singleton 0)
                    . traverse executeInstruction $ instructions

part1 ∷ [(CardinalDir, Int)] → Int
part1 = tailVisitCount 2

part2 ∷ [(CardinalDir, Int)] → Int
part2 = tailVisitCount 10

main ∷ IO ()
main = do
  aocMain "inputs/09.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
