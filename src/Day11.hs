{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day11 (main) where

import           AOC
import           AOC.Parse
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict       ((!))
import qualified Data.Map.Strict       as M
import qualified Data.Map.Strict       as Map
import           PyF                   (str)
import           Relude.Unsafe         ((!!))
import           Text.Megaparsec.Char  (hspace1, newline, string)


-- I'd prefer to use functions, but I want to derive Show for monkey
data Operation = Plus Int
               | Times Int
               | TimesOld
  deriving (Show)

runOp ∷ Operation → Int → Int
runOp op old = case op of
  Plus  x  -> old + x
  Times x  -> old * x
  TimesOld -> old * old

newtype Test = DivisibleBy { divisor :: Int }
  deriving (Eq, Ord, Show)

runTest ∷ Test → Int → Bool
runTest (DivisibleBy x) n = (n `mod` x) == 0

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
  deriving (Show)

-- Keyed by monkey id
-- Information about how the monkeys behave. This does not change after initial setup
type Monkeys = Map Int Monkey

-- Keyed by monkey id
-- Information about the current state of the monkeys - their current held items and
-- inspection count
type MonkeyStates = Map Int MonkeyState


-- |~) _  _ _. _  _
-- |~ (_|| _\|| |(_|
--                _|
parseInput ∷ Text → Monkeys
parseInput = M.fromList . map (\m → (m.index, m)) . unsafeParse
  (monkeyP `sepEndBy` (try (void $ newline <* newline) <|> eof))

monkeyP ∷ Parser Monkey
monkeyP = do
  index <- string "Monkey " *> decimal <* single ':' <* newline
  _ <- hspace1 *> string "Starting items: "
  initialItems <- decimal `sepBy` string ", " <* newline
  _ <- hspace1 *> string "Operation: new = old "
  operation <- binOp
  _ <- newline <* hspace1 <* string "Test: divisible by "
  test <- DivisibleBy <$> decimal
  _ <- newline <* hspace1 <* string "If true: throw to monkey "
  throwToIfTrue <- decimal
  _ <- newline <* hspace1 <* string "If false: throw to monkey "
  throwToIfFalse <- decimal
  pure Monkey {..}

binOp ∷ Parser Operation
binOp = try (Plus <$ single '+' <*> (hspace1 *> decimal))
    <|> try (Times <$ single '*' <*> (hspace1 *> decimal))
    <|>     (TimesOld <$ string "* old")


-- |~) _  __|_  /~\ _  _
-- |~ (_||  |   \_/| |(/_
--
part1 ∷ Monkeys → Int
part1 = monkeyBusinessLvl (`div` 3) 20

monkeyBusinessLvl ∷ (Int → Int) → Int → Monkeys → Int
monkeyBusinessLvl shrinkWorry nRounds monkeys = product . take 2 . sortOn Down $ inspectionCounts
  where
  s0 = initMonkeyStates monkeys
  s1 = iterate (runRound monkeys shrinkWorry) s0 !! nRounds
  inspectionCounts = (.inspectionCount) <$> Map.elems s1

initMonkeyStates ∷ Monkeys → MonkeyStates
initMonkeyStates = M.map \m → MonkeyState m.initialItems 0

runMonkeyTurn ∷ Monkeys → (Int → Int) → MonkeyStates → Int → MonkeyStates
runMonkeyTurn monkeys shrinkWorry states ix = Map.insert ix currMonkeyState'
                                            . mergeInNewItemLocations newItemLocations
                                            $ states
  where
    currMonkeyState  = states ! ix
    currMonkeyState' = MonkeyState mempty
                     ( currMonkeyState.inspectionCount
                     + length currMonkeyState.currentItems)

    newItemLocations ∷ Map Int [Int]
    newItemLocations = Map.fromListWith (<>)
                     . map (second (:[]) . decideThrow)
                     $ currMonkeyState.currentItems

    decideThrow ∷ Int → (Int, Int)
    decideThrow item =
        let monkey = monkeys ! ix
            worryLevel = shrinkWorry $ runOp monkey.operation item
            throwTo | runTest monkey.test worryLevel = monkey.throwToIfTrue
                    | otherwise                      = monkey.throwToIfFalse
        in (throwTo, worryLevel)

    mergeInNewItemLocations ∷ Map Int [Int] → MonkeyStates → MonkeyStates
    mergeInNewItemLocations = Merge.merge
      Merge.dropMissing -- There should never be keys in the new item locations
                        -- that aren't in the monkey states
      Merge.preserveMissing
      (Merge.zipWithMatched \_ newItems (MonkeyState is n) → MonkeyState (is <> newItems) n)

runRound ∷ Monkeys → (Int → Int) → MonkeyStates → MonkeyStates
runRound monkeys shrinkWorry s0 = foldl' (runMonkeyTurn monkeys shrinkWorry) s0 [0..nMonkeys-1]
  where
    nMonkeys = Map.size s0


-- |~) _  __|_  ~|~  _
-- |~ (_||  |    |VV(_)
--
part2 ∷ Monkeys → Int
part2 ms = monkeyBusinessLvl (`mod` commonMultiple) 10000 ms
  where
    commonMultiple = product . M.map ((.divisor) . (.test)) $ ms


-- |\/| _ . _
-- |  |(_||| |
--
main ∷ IO ()
main = do
  aocMain "inputs/11.txt" Solution { parse=parseInput, part1=part1, part2=part2 }


-- (~   _  _ _  _ | _   . _  _   _|_
-- (_><(_|| | ||_)|(/_  || ||_)|_||
--             |            |
exampleInput ∷ Text
exampleInput = toText @String [str|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1|]

