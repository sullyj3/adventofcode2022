{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day11 (main) where

import           AOC
import           AOC.Parse
import           Data.List            (partition)
import           Data.Map.Strict      ((!))
import qualified Data.Map.Strict      as M
import qualified Data.Map.Strict      as Map
import           Relude.Unsafe        ((!!))
import           Text.Megaparsec.Char (hspace1, newline, string)
import Optics.Core ((%~), (%), ix, (.~))


-- I'd prefer to use functions, but I want to derive Show for monkey
data Operation = Plus Int
               | Times Int
               | TimesOld
  deriving (Show)

newtype Test = DivisibleBy { divisor :: Int }
  deriving (Eq, Ord, Show)

data Monkey = Monkey { index          :: Int
                     , initialItems   :: [Int]
                     , operation      :: Operation
                     , test           :: Test
                     , throwToIfTrue  :: Int
                     , throwToIfFalse :: Int
                     }
  deriving (Show)

data MonkeyState = MonkeyState { currentItems    :: ![Int]
                               , inspectionCount :: !Int
                               }
  deriving (Show, Generic)

-- Information about how the monkeys behave. This does not change after initial setup
type Monkeys = [Monkey]

-- Keyed by monkey id
-- Information about the current state of the monkeys - their current held items and
-- inspection count
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
  test <- DivisibleBy <$> decimal <* newline

  _ <- hspace1 <* string "If true: throw to monkey "
  throwToIfTrue <- decimal <* newline

  _ <- hspace1 <* string "If false: throw to monkey "
  throwToIfFalse <- decimal <* newline

  pure Monkey {..}

binOp ∷ Parser Operation
binOp = choice [ try $ Plus <$ single '+' <*> (hspace1 *> decimal)
               , try $ Times <$ single '*' <*> (hspace1 *> decimal)
               ,       TimesOld <$ string "* old"
               ]


-- |~) _  __|_  /~\ _  _
-- |~ (_||  |   \_/| |(/_
--
part1 ∷ Monkeys → Int
part1 = monkeyBusinessLvl (`div` 3) 20

monkeyBusinessLvl ∷ (Int → Int) → Int → Monkeys → Int
monkeyBusinessLvl shrinkWorry nRounds monkeys = product . take 2 . sortOn Down $ inspectionCounts
  where
    initialStates = M.fromList $
      (\Monkey {index, initialItems} → (index, MonkeyState initialItems 0)) <$> monkeys
    finalStates = iterate (runRound monkeys shrinkWorry) initialStates !! nRounds
    inspectionCounts = (.inspectionCount) <$> Map.elems finalStates

runMonkeyTurn ∷ (Int → Int) → MonkeyStates → Monkey → MonkeyStates
runMonkeyTurn shrinkWorry states monkey = states
  & ix monkey.throwToIfFalse % #currentItems %~ (<> falses)
  & ix monkey.throwToIfTrue  % #currentItems %~ (<> trues)
  & ix monkey.index .~ MonkeyState [] (inspectionCount + length currentItems)
  where
    MonkeyState {currentItems, inspectionCount} = states ! monkey.index
    worryLevels = shrinkWorry . runOp monkey.operation <$> currentItems
    (trues, falses) = partition (runTest monkey.test) worryLevels

    runOp op old = case op of
      Plus  x  -> old + x
      Times x  -> old * x
      TimesOld -> old * old

    runTest (DivisibleBy x) n = (n `mod` x) == 0

runRound ∷ Monkeys → (Int → Int) → MonkeyStates → MonkeyStates
runRound monkeys shrinkWorry initialStates =
  foldl' (runMonkeyTurn shrinkWorry) initialStates monkeys


-- |~) _  __|_  ~|~  _
-- |~ (_||  |    |VV(_)
--
part2 ∷ Monkeys → Int
part2 ms = monkeyBusinessLvl (`mod` commonMultiple) 10000 ms
  where
    commonMultiple = product . map ((.divisor) . (.test)) $ ms


-- |\/| _ . _
-- |  |(_||| |
--
main ∷ IO ()
main = do
  aocMain "inputs/11.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
