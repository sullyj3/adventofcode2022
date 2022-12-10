module Day10 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import           Extra                      (chunksOf)
import           PyF
import           Text.Megaparsec.Char       (string)
import           Text.Megaparsec.Char.Lexer (signed)
import           Utils                      (selectIndices)

data Instruction = Noop
                 | Addx Int
  deriving (Eq, Show)
-------------
-- Parsing --
-------------
parseInput ∷ Text → [Instruction]
parseInput = unsafeParse (linesOf instructionP)
  where
    instructionP = try (Noop <$ string "noop") <|> (Addx <$> (string "addx " *> signedInt))

signedInt ∷ Parser Int
signedInt = signed (pure ()) decimal

---------------
-- Solutions --
---------------

--
-- Part 1
--
part1 ∷ [Instruction] → Int
part1 = sum
  . selectIndices [i-1 | i <- [20,60,100,140,180,220]]
  . signalStrengths
  . xValues

xValues ∷ [Instruction] → [Int]
xValues = scanl (+) 1 . instructionsToAdds

instructionsToAdds ∷ [Instruction] → [Int]
instructionsToAdds = concatMap \case
  Noop   -> [0]
  Addx x -> [0, x]

signalStrengths ∷ [Int] → [Int]
signalStrengths = zipWith (*) [1..]

--
-- Part 2
--
part2 ∷ [Instruction] → Text
part2 instructions = unlines . fmap toText
  . zipWith (zipWith renderPixel) rowIndices
  $ chunkedXs
  where
    chunkedXs = chunksOf 40 $ xValues instructions
    rowIndices = repeat [0..40-1]
    renderPixel rowIndex x
      | abs (x - rowIndex) <= 1 = '#'
      | otherwise = '.'

main ∷ IO ()
main = do
  putStrLn "Part 1:"
  aocSinglePartMain "inputs/10.txt" exampleInput parseInput part1

  putStrLn ""
  putStrLn "Part 2:"
  input <- parseInput <$> readFileText "inputs/10.txt"
  putTextLn $ part2 input

exampleInput ∷ Text
exampleInput = toText @String [str|addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop|]
