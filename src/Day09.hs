module Day09 (main) where

import           AOC
import           AOC.Parse            hiding (State)
import           AOC.Parsers
import qualified Data.Set             as Set
import qualified Relude.Unsafe        as Unsafe
import           Text.Megaparsec.Char (upperChar)
import           Utils                ((.:))

data Direction = U | D | L | R deriving (Show, Eq)
type Coord = (Int, Int)
type VisitedSet = Set Coord
type Rope = [Coord]

-------------
-- Parsing --
-------------
parseInput ∷ Text → [(Direction, Int)]
parseInput = unsafeParse $ linesOf $ pairOfBoth direction decimal " "
  where
    direction ∷ Parser Direction
    direction = upperChar >>= \case
      'U' -> pure U
      'D' -> pure D
      'L' -> pure L
      'R' -> pure R
      c   -> fail $ "Invalid direction: " <> [c]

---------------
-- Solutions --
---------------
(<+>) ∷ (Num a, Num b) ⇒ (a, b) → (a, b) → (a, b)
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(<->) ∷ (Num a, Num b) ⇒ (a, b) → (a, b) → (a, b)
(<->) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

updateChild ∷ Coord → Coord → Coord
updateChild newParent oldChild@(ocx, ocy)
  | touching = oldChild
  | otherwise = (ocx + signum offsetX, ocy + signum offsetY)
  where
    (offsetX, offsetY) = newParent <-> oldChild
    touching = max (abs offsetX) (abs offsetY) <= 1

-- returns the new location of the tail
updateRope ∷ Coord → Rope → (Coord, Rope)
updateRope _______ [] = error "updateRope: empty rope"
updateRope newHead (_:rope) = (newTail, newHead:rope')
  where
    (newTail, rope') = mapAccumL (join (,) .: updateChild) newHead rope

executeInstruction ∷ MonadState (Rope, VisitedSet) m ⇒ (Direction, Int) → m ()
executeInstruction instruction = do
  (rope, visited) <- get

  let oldHead = Unsafe.head rope
      (tailLocations, rope') = flip runState rope $
        traverse (state . updateRope . (oldHead <+>))
               $ instructionOffsets instruction

  put (rope', Set.fromList tailLocations <> visited)
  where
    instructionOffsets ∷ (Direction, Int) → [Coord]
    instructionOffsets (dir, steps) = case dir of
      U -> [ (0, y) | y <- [1 .. steps] ]
      D -> [ (0, y) | y <- [-1, -2 .. -steps] ]
      R -> [ (x, 0) | x <- [1 .. steps] ]
      L -> [ (x, 0) | x <- [-1, -2 .. -steps] ]

tailVisits ∷ Int → [(Direction, Int)] → Int
tailVisits ropeLength instructions = Set.size visited
  where
    (_,visited) = flip execState (replicate ropeLength (0,0), Set.singleton (0,0))
                    . traverse executeInstruction $ instructions

part1 ∷ [(Direction, Int)] → Int
part1 = tailVisits 2

part2 ∷ [(Direction, Int)] → Int
part2 = tailVisits 10

main ∷ IO ()
main = do
  aocMain "inputs/09.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
