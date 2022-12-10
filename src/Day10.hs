module Day10 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import           Extra                      (chunksOf)
import           PyF
import           Text.Megaparsec.Char       (string)
import           Utils                      (imap1, selectIndices1)
import Optics.Core (imap)

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

---------------
-- Solutions --
---------------

--
-- Part 1
--
part1 ∷ [Instruction] → Int
part1 = sum . selectIndices1 [20,60..220] . imap1 (*) . xValues

xValues ∷ [Instruction] → [Int]
xValues = scanl (+) 1 . concatMap \case
  Noop   -> [0]
  Addx x -> [0, x]

--
-- Part 2
--
part2 ∷ [Instruction] → Text
part2 = unlines . map (toText . imap renderPixel) . chunksOf 40 . xValues
  where
    renderPixel ix x = if abs (x - ix) <= 1 then '█' else ' '

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
