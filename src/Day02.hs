module Day02 (main) where
import Utils (parsePair, unreachable, parsePair2)
import qualified Data.Text.IO as T

class (Eq a, Enum a, Bounded a) ⇒ Cyclic a where
  succCyclic ∷ a → a
  succCyclic x | x == maxBound = minBound
               | otherwise = succ x

  predCyclic ∷ a → a
  predCyclic x | x == minBound = maxBound
               | otherwise = pred x

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show, Ord, Enum, Bounded)

data Result = Loss | Draw | Win
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Cyclic RPS

parseResult ∷ (Eq a, IsString a) => a → Result
parseResult c = case c of
  "X" → Loss
  "Y" → Draw
  "Z" → Win
  _ → unreachable

parseRPS ∷ (Eq a, IsString a) => a → RPS
parseRPS c = case c of
  "A" → Rock
  "B" → Paper
  "C" → Scissors
  "X" → Rock
  "Y" → Paper
  "Z" → Scissors
  _ → unreachable

solvePart1 ∷ Text → Int
solvePart1 = sum . map (uncurry scorePart1) . parsePart1
  where
    parsePart1Line = parsePair parseRPS " "

    parsePart1 ∷ Text → [(RPS, RPS)]
    parsePart1 = map parsePart1Line . lines
    scorePart1 them us = resultScore (result us them) + shapeScore us
    
solvePart2 :: Text -> Int
solvePart2 = sum . map (uncurry scorePart2) . parsePart2
  where
    parsePart2Line = parsePair2 parseRPS parseResult " "
    parsePart2 = map parsePart2Line . lines

    scorePart2 them desiredResult = shapeScore ourPlay + resultScore desiredResult
      where   
        ourPlay = reverseEngineerPlay them desiredResult

reverseEngineerPlay ∷ RPS → Result → RPS
reverseEngineerPlay them desiredResult = case desiredResult of
  Draw → them
  -- the data type is defined in order Rock, Paper, Scissors
  -- if we consider the cycle of those constructors, each one
  -- beats its predecessor
  Win → succCyclic them
  Loss → predCyclic them
  
result ∷ RPS → RPS → Result
result us them = toEnum $ (fromEnum us - fromEnum them + 1) `mod` 3

shapeScore ∷ RPS → Int
shapeScore us = fromEnum us + 1

resultScore ∷ Result → Int
resultScore theResult = 3 * fromEnum theResult

main :: IO ()
main = do
  contents ← T.readFile "inputs/day02.txt"
  print $ solvePart1 contents
  print $ solvePart2 contents
