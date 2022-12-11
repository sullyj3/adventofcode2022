{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day11 (main) where

import           AOC
import           AOC.Parse
import           Data.Map.Strict      ((!))
import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import           Data.Strict          (Pair)
import           Data.Strict.Tuple    (Pair (..))
import           PyF                  (str)
import           Relude.Unsafe        ((!!))
import           Text.Megaparsec.Char (hspace1, newline, string)

-- I'd prefer to use functions, but I want to derive Show for monkey
data Operation = Plus !Integer
               | Times !Integer
               | TimesOld
  deriving (Eq, Ord, Show)

runOp ∷ Operation → Integer → Integer
runOp op old = case op of
  Plus  x  -> old + x
  Times x  -> old * x
  TimesOld -> old * old

newtype Test = DivisibleBy { divisor :: Integer }
  deriving (Eq, Ord, Show)

runTest ∷ Test → Integer → Bool
runTest = \case
  DivisibleBy x -> (== 0) . (`mod` x)

data Monkey = Monkey { index          :: Int
                     , initialItems   :: [Integer]
                     , operation      :: Operation
                     , test           :: Test
                     , throwToIfTrue  :: Int
                     , throwToIfFalse :: Int
                     }
  deriving (Eq, Ord, Show)

data MonkeyState = MonkeyState { currentItems    :: !(Seq Integer)
                               , inspectionCount :: !Int
                               }
  deriving (Eq, Ord, Show)

instance Semigroup MonkeyState where
  (<>) :: MonkeyState → MonkeyState → MonkeyState
  MonkeyState items1 count1 <> MonkeyState items2 count2 =
    MonkeyState (items1 <> items2) (count1 + count2)

instance Monoid MonkeyState where
  mempty :: MonkeyState
  mempty = MonkeyState mempty 0

type Monkeys = Map Int Monkey
type MonkeyStates = Map Int MonkeyState

-- |~) _  _ _. _  _
-- |~ (_|| _\|| |(_|
--                _|
parseInput ∷ Text → [Monkey]
parseInput = unsafeParse $
  monkeyP `sepEndBy` (try (void $ newline <* newline) <|> eof)

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
common ∷ (Integer → Integer) → Int → [Monkey] → Int
common shrinkWorry nRounds ms = product . take 2 . sortOn Down $ inspectionCounts
  where
  monkeys = initMonkeys ms
  s0 = initMonkeyStates ms
  s1 = iterate (runRound monkeys shrinkWorry) s0 !! nRounds
  inspectionCounts = (.inspectionCount) <$> Map.elems s1

part1 ∷ [Monkey] → Int
part1 = common (`div` 3) 20

initMonkeyStates ∷ [Monkey] → MonkeyStates
initMonkeyStates = fromList . map \m → (m.index, MonkeyState (Seq.fromList m.initialItems) 0)

initMonkeys ∷ [Monkey] → Monkeys
initMonkeys = fromList . map \m → (m.index, m)

runMonkeyTurn ∷ Monkeys → (Integer → Integer) → MonkeyStates → Int → MonkeyStates
runMonkeyTurn monkeys shrinkWorry states ix = states'
  where
    s0 = states ! ix
    monkey = monkeys ! ix

    newItemLocations = map decideItem (toList s0.currentItems)

    decideItem item =
        let worryLevel = shrinkWorry $ runOp monkey.operation item
            throwTo | runTest monkey.test worryLevel = monkey.throwToIfTrue
                    | otherwise                      = monkey.throwToIfFalse
        in (throwTo :!: worryLevel)

    s1 = MonkeyState Seq.empty (s0.inspectionCount + Seq.length s0.currentItems)

    states' ∷ MonkeyStates
    states' = Map.unionWith (<>) (itemLocationsToStates newItemLocations)
            . Map.insert ix s1
            $ states

    itemLocationsToStates ∷ [Pair Int Integer] → MonkeyStates
    itemLocationsToStates = Map.fromListWith (<>) . toList . fmap \case
        (throwTo :!: item) -> (throwTo, MonkeyState (Seq.singleton item) 0)

runRound ∷ Monkeys → (Integer → Integer) → MonkeyStates → MonkeyStates
runRound monkeys shrinkWorry s0 = foldl' (runMonkeyTurn monkeys shrinkWorry) s0 [0..nMonkeys-1]
  where
    nMonkeys = Map.size s0

-- |~) _  __|_  ~|~  _
-- |~ (_||  |    |VV(_)
--
-- >>> part2 . parseInput $ exampleInput
part2 ∷ [Monkey] → Int
part2 ms = common (`mod` commonMultiple) 10000 ms
  where
    commonMultiple = product . map ((.divisor) . (.test)) $ ms


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

