module Day09 (main) where

import           AOC
import           AOC.Parse hiding (State)
import           AOC.Parsers
import           PyF
import Text.Megaparsec.Char (upperChar)
import qualified Data.Set as Set

data Direction = U | D | L | R deriving (Show, Eq)
newtype Head = Head { unHead :: (Int, Int)} deriving (Show, Eq, Ord)
newtype Tail = Tail { unTail :: (Int, Int)} deriving (Show, Eq, Ord)
type VisitedSet = Set Tail

-------------
-- Parsing --
-------------
-- parseInput = id
parseInput :: Text -> [(Direction, Int)]
parseInput = unsafeParse $ linesOf $ pairOfBoth direction decimal " "
  where
    direction :: Parser Direction
    direction = upperChar >>= \case
      'U' -> pure U
      'D' -> pure D
      'L' -> pure L
      'R' -> pure R
      c   -> fail $ "Invalid direction: " <> [c]
-- parseInput = toString

---------------
-- Solutions --
---------------
touching :: Head -> Tail -> Bool
touching (Head (x1, y1)) (Tail (x2, y2)) = max (abs (x1 - x2)) (abs (y1 - y2)) <= 1

updateTail :: Head -> Head -> Tail -> Tail
updateTail oldHead newHead oldTail
  | touching newHead oldTail = oldTail
  | otherwise = let Head h = oldHead in Tail h

headMovesTo :: (Int, Int) -> State (Head, Tail) Tail
headMovesTo (x, y) = do
  (oldHead, oldTail) <- get
  let newHead = Head (x, y)
      newTail = updateTail oldHead newHead oldTail
  -- traceM $ "head: " <> show oldHead.unHead <> " -> " <> show newHead.unHead
  --      <> ", tail: " <> show oldTail.unTail <> " -> " <> show newTail.unTail
  put (newHead, newTail)
  pure newTail

executeInstruction :: (Direction, Int) -> State (Head, Tail, VisitedSet) ()
executeInstruction (dir, steps) = do
  -- traceM $ "executing instruction: " <> show dir <> " " <> show steps
  (h@(Head (hx, hy)), t, visited) <- get
  -- traceM $ "head is at " <> show h <> ", tail is at " <> show t

  let headVisits :: [(Int, Int)]
      headVisits = case dir of
        U -> [ (hx, y) | y <- [hy+1      .. hy+steps] ]
        D -> [ (hx, y) | y <- [hy-1,hy-2 .. hy-steps] ]
        R -> [ (x, hy) | x <- [hx+1      .. hx+steps] ]
        L -> [ (x, hy) | x <- [hx-1,hx-2 .. hx-steps] ]
  -- traceM $ "head will visit: " <> show headVisits

  let (tailLocations, (h', t')) = flip runState (h, t) . traverse headMovesTo $ headVisits
  put (h', t', Set.fromList tailLocations <> visited)


part1 :: [(Direction, Int)] -> Int
part1 instructions = Set.size visited
  where 
    (_,_,visited) = flip execState (Head (0,0), Tail (0,0), Set.singleton (Tail (0,0)))
                    . traverse executeInstruction $ instructions


part2 = const ()

main ∷ IO ()
main = do
  -- other testing here
  -- aocSinglePartExample parseInput part1 exampleInput
  aocSinglePartMain "inputs/09.txt" exampleInput parseInput part1

  -- aocMain "inputs/09.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

exampleInput :: Text
exampleInput = toText @String [str|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|]

