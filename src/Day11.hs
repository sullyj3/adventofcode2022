{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day11 (main) where

import           AOC
import           AOC.Parse
import           Data.List            (partition)
import           Data.Map.Strict      ((!))
import qualified Data.Map.Strict      as Map
import           Optics.Core          (ix, (%), (%~), (.~))
import           Relude.Unsafe        ((!!))
import           Text.Megaparsec.Char (hspace1, newline, string)
import           Text.Show.Functions  ()
import           Utils                (divides, nTimes)


data Monkey = Monkey { index          :: Int
                     , initialItems   :: [Int]
                     , operation      :: Int -> Int
                     , divisor        :: Int
                     , throwToIfTrue  :: Int
                     , throwToIfFalse :: Int
                     }
  deriving (Show)

data MonkeyState = MonkeyState { currentItems    :: ![Int]
                               , inspectionCount :: !Int
                               }
  deriving (Generic, Show)

-- Information about how the monkeys behave. This does not change after initial 
-- setup
type Monkeys = [Monkey]

-- Keyed by monkey id
-- Information about the current state of the monkeys - their current held 
-- items and inspection count
type MonkeyStates = Map Int MonkeyState


-- |~) _  _ _. _  _
-- |~ (_|| _\|| |(_|
--                _|
parseInput ∷ Text → Monkeys
parseInput = unsafeParse $
  monkeyP `sepEndBy` (try (void newline) <|> eof)

monkeyP ∷ Parser Monkey
monkeyP = do
  index <- string "Monkey " *> decimal <* single ':' <* newline

  _ <- hspace1 *> string "Starting items: "
  initialItems <- decimal `sepBy` string ", " <* newline

  _ <- hspace1 *> string "Operation: new = old "
  operation <- binOp <* newline

  _ <- hspace1 <* string "Test: divisible by "
  divisor <- decimal <* newline

  _ <- hspace1 <* string "If true: throw to monkey "
  throwToIfTrue <- decimal <* newline

  _ <- hspace1 <* string "If false: throw to monkey "
  throwToIfFalse <- decimal <* newline

  pure Monkey {..}

binOp ∷ Parser (Int → Int)
binOp = choice [ try $ (+) <$ single '+' <*> (hspace1 *> decimal)
               , try $ (*) <$ single '*' <*> (hspace1 *> decimal)
               ,  join (*) <$ string "* old"
               ]


-- |~) _  __|_  /~\ _  _
-- |~ (_||  |   \_/| |(/_
--
part1 ∷ Monkeys → Int
part1 = monkeyBusinessLvl (`div` 3) 20

monkeyBusinessLvl ∷ (Int → Int) → Int → Monkeys → Int
monkeyBusinessLvl shrinkWorry nRounds monkeys =
  product . take 2 . sortOn Down $ inspectionCounts
  where
    initialStates = Map.fromList $
      (\monkey → (monkey.index, MonkeyState monkey.initialItems 0)) <$> monkeys
    finalStates = nTimes nRounds (runRound monkeys shrinkWorry) initialStates
    inspectionCounts = (.inspectionCount) <$> Map.elems finalStates

runMonkeyTurn ∷ (Int → Int) → MonkeyStates → Monkey → MonkeyStates
runMonkeyTurn shrinkWorry states monkey = states
  & ix monkey.throwToIfFalse % #currentItems %~ (<> falses)
  & ix monkey.throwToIfTrue  % #currentItems %~ (<> trues)
  & ix monkey.index .~ MonkeyState [] (inspectionCount + length currentItems)
  where
    MonkeyState {currentItems, inspectionCount} = states ! monkey.index
    worryLevels = shrinkWorry . monkey.operation <$> currentItems
    (trues, falses) = partition (monkey.divisor `divides`) worryLevels

runRound ∷ Monkeys → (Int → Int) → MonkeyStates → MonkeyStates
runRound monkeys shrinkWorry initialStates =
  foldl' (runMonkeyTurn shrinkWorry) initialStates monkeys


-- |~) _  __|_  ~|~  _
-- |~ (_||  |    |VV(_)
--
part2 ∷ Monkeys → Int
part2 ms = monkeyBusinessLvl (`mod` commonMultiple) 10000 ms
  where
    commonMultiple = product . map (.divisor) $ ms


-- |\/| _ . _
-- |  |(_||| |
--
main ∷ IO ()
main = do
  aocMain "inputs/11.txt" 
          Solution { parse=parseInput, part1=part1, part2=part2 }
