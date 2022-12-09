module Day09 (main) where

import           AOC
import           AOC.Parse hiding (State)
import           AOC.Parsers
import           PyF
import Text.Megaparsec.Char (upperChar)
import qualified Data.Set as Set
import Data.Tuple.Optics (Field1(_1))
import qualified Relude.Unsafe as Unsafe
import Data.Traversable (for)

data Direction = U | D | L | R deriving (Show, Eq)
type Coord = (Int, Int)
type VisitedSet = Set Coord
type Rope = [Coord]

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
updateChild :: Coord -> Coord -> Coord
updateChild newParent oldChild
  | touching = oldChild
  -- not touching
  -- same row
  | offsetY == 0 && abs offsetX >= 2 = (ocx + signum offsetX, ocy)
  -- same column
  | offsetX == 0 && abs offsetY >= 2 = (ocx, ocy + signum offsetY)
  -- not same row or column, need to move diagonally
  | otherwise = (ocx + signum offsetX, ocy + signum offsetY)
  where
    (npx, npy) = newParent
    (ocx, ocy) = oldChild
    (offsetX, offsetY) = (npx - ocx, npy - ocy)
    touching = max (abs (npx - ocx)) (abs (npy - ocy)) <= 1

-- returns the new location of the tail
updateRope :: Coord -> Rope -> (Coord, Rope)
updateRope _______ [] = error "updateRope: empty rope"
updateRope newHead (oldHead:knots) = (newTail, newHead:knots')
  where
    ((_oldTail, newTail), knots') = mapAccumL go (oldHead, newHead) knots

    -- Sorry
    go :: (Coord, Coord) -> Coord -> ((Coord, Coord), Coord)
    go (oldParent, newParent) oldChild = ((oldChild, newChild) , newChild)
        where newChild = updateChild newParent oldChild

executeInstruction :: MonadState (Rope, VisitedSet) m => (Direction, Int) -> m ()
executeInstruction (dir, steps) = do
  (rope, visited) <- get
  let (hx, hy) = Unsafe.head rope

  let headVisits :: [(Int, Int)]
      headVisits = case dir of
        U -> [ (hx, y) | y <- [hy+1      .. hy+steps] ]
        D -> [ (hx, y) | y <- [hy-1,hy-2 .. hy-steps] ]
        R -> [ (x, hy) | x <- [hx+1      .. hx+steps] ]
        L -> [ (x, hy) | x <- [hx-1,hx-2 .. hx-steps] ]
  -- traceM $ "head will visit: " <> show headVisits

  let (tailLocations, rope') = flip runState rope $ for headVisits \loc -> do
        currRope <- get
        -- traceM $ "curr rope is " <> show currRope
        state $ updateRope loc
  put (rope', Set.fromList tailLocations <> visited)


part1 :: [(Direction, Int)] -> Int
part1 instructions = Set.size visited
  where 
    (_,visited) = flip execState ([(0,0), (0,0)], Set.singleton (0,0))
                    . traverse executeInstruction $ instructions

part2 :: [(Direction, Int)] -> Int
part2 instructions = Set.size visited
  where 
    initialRope = replicate 10 (0,0)
    (_,visited) = flip execState (initialRope, Set.singleton (0,0))
                    . traverse executeInstruction $ instructions

part2StepByStep :: [(Direction, Int)] -> IO Int
part2StepByStep instructions = do
  let initialRope = replicate 10 (0,0)
      initialVisited = Set.singleton (0,0)
  visited <- flip evalStateT (initialRope, initialVisited) $ do
    liftIO $ putStrLn "Starting"
    liftIO $ putStrLn $ "Instructions are " <> show instructions
    for_ instructions \instruction -> do
      (rope, visited) <- get
      liftIO $ putStrLn $ "Rope is " <> show rope
      liftIO $ putStrLn $ "Visited is " <> show visited
      liftIO $ putStrLn $ "Executing instruction " <> show instruction
      executeInstruction instruction
      (rope', visited') <- get
      liftIO $ putStrLn $ "Rope is now " <> show rope'
      liftIO $ putStrLn $ "Visited is now " <> show visited'
      liftIO $ putStrLn "press enter to continue"
      liftIO getLine
    gets snd
  pure $ Set.size visited


main ∷ IO ()
main = do
  -- other testing here
  -- aocSinglePartExample parseInput part1 exampleInput
  aocSinglePartMain "inputs/09.txt" largerExample parseInput part2
  -- _ <- part2StepByStep (parseInput exampleInput)
  pure ()

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

largerExample :: Text
largerExample = toText @String [str|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20|]
