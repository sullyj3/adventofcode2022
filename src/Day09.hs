module Day09 (main) where

import           AOC
import           AOC.Parse            hiding (State)
import           AOC.Parsers
import qualified Data.Set             as Set
import qualified Relude.Unsafe        as Unsafe
import           Text.Megaparsec.Char (upperChar)
import Utils ((.:))

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
updateChild ∷ Coord → Coord → Coord
updateChild newParent oldChild
  | touching = oldChild
  | otherwise = (ocx + signum offsetX, ocy + signum offsetY)
  where
    (npx, npy) = newParent
    (ocx, ocy) = oldChild
    (offsetX, offsetY) = (npx - ocx, npy - ocy)
    touching = max (abs (npx - ocx)) (abs (npy - ocy)) <= 1

-- returns the new location of the tail
updateRope ∷ Coord → Rope → (Coord, Rope)
updateRope _______ [] = error "updateRope: empty rope"
updateRope newHead (_:rope) = (newTail, newHead:rope')
  where
    (newTail, rope') = mapAccumL (join (,) .: updateChild) newHead rope

executeInstruction ∷ MonadState (Rope, VisitedSet) m ⇒ (Direction, Int) → m ()
executeInstruction (dir, steps) = do
  (rope, visited) <- get

  let (hx, hy) = Unsafe.head rope
      headVisits ∷ [(Int, Int)]
      headVisits = case dir of
        U -> [ (hx, y) | y <- [hy+1      .. hy+steps] ]
        D -> [ (hx, y) | y <- [hy-1,hy-2 .. hy-steps] ]
        R -> [ (x, hy) | x <- [hx+1      .. hx+steps] ]
        L -> [ (x, hy) | x <- [hx-1,hx-2 .. hx-steps] ]
      (tailLocations, rope') = flip runState rope $
        traverse (state . updateRope) headVisits 

  put (rope', Set.fromList tailLocations <> visited)

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
