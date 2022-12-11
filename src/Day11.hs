{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE NoFieldSelectors #-}
module Day11 (main) where

import           AOC
import           AOC.Parse
import           PyF                  (str)
import           Text.Megaparsec.Char (hspace1, newline, string)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Utils (prettyMap)
import Relude.Unsafe ((!!))
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

-- I'd prefer to use functions, but I want to derive Show for monkey
data Operation = Plus Integer
               | Times Integer
               | TimesOld
  deriving (Eq, Ord, Show)

runOp :: Operation -> Integer -> Integer
runOp op old = case op of
  Plus  x -> old + x
  Times x -> old * x
  TimesOld -> old * old

newtype Test = DivisibleBy Integer
  deriving (Eq, Ord, Show)

runTest :: Test -> Integer -> Bool
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

type Monkeys = Map Int Monkey
type MonkeyStates = Map Int MonkeyState

-- |~) _  _ _. _  _
-- |~ (_|| _\|| |(_|
--                _|
-- >>> parseInput $ exampleInput
-- [Monkey {index = 0, initialItems = [79,98], operation = Times 19, test = DivisibleBy 23, throwToIfTrue = 2, throwToIfFalse = 3},Monkey {index = 1, initialItems = [54,65,75,74], operation = Plus 6, test = DivisibleBy 19, throwToIfTrue = 2, throwToIfFalse = 0},Monkey {index = 2, initialItems = [79,60,97], operation = TimesOld, test = DivisibleBy 13, throwToIfTrue = 1, throwToIfFalse = 3},Monkey {index = 3, initialItems = [74], operation = Plus 3, test = DivisibleBy 17, throwToIfTrue = 0, throwToIfFalse = 1}]
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
-- >>> part1 . parseInput $ exampleInput
common ∷ (Integer -> Integer) -> Int -> [Monkey] → Int
common maybeShrinkWorry nRounds ms = product . take 2 . sortOn Down $ inspectionCounts
  where
  monkeys = initMonkeys ms
  s0 = initMonkeyStates ms
  s1 = iterate (runRound monkeys maybeShrinkWorry) s0 !! nRounds
  inspectionCounts = (.inspectionCount) <$> Map.elems s1

part1 ∷ [Monkey] → Int
part1 = common (`div` 3) 20

initMonkeyStates ∷ [Monkey] → MonkeyStates
initMonkeyStates = fromList . map \m → (m.index, MonkeyState (Seq.fromList m.initialItems) 0)

initMonkeys ∷ [Monkey] → Monkeys
initMonkeys = fromList . map \m → (m.index, m)

runMonkeyTurn ∷ Monkeys -> (Integer -> Integer) -> MonkeyStates -> Int -> MonkeyStates
runMonkeyTurn monkeys maybeShrinkWorry states ix = flip execState states $ do
  for_ s0.currentItems \item -> do
    let worryLevel = maybeShrinkWorry $ runOp monkey.operation item
        throwTo | runTest monkey.test worryLevel = monkey.throwToIfTrue
                | otherwise                      = monkey.throwToIfFalse
    modifyMonkeyState (\s -> s { currentItems = s.currentItems |> worryLevel }) throwTo 
  let s1 = MonkeyState Seq.empty (s0.inspectionCount + fromIntegral (length s0.currentItems))
  modifyMonkeyState (const s1) ix
  where
    s0 = states ! ix
    monkey = monkeys ! ix
    modifyMonkeyState :: (MonkeyState -> MonkeyState) -> Int -> State MonkeyStates ()
    modifyMonkeyState f = modify . Map.adjust f

runRound ∷ Monkeys -> (Integer -> Integer) -> MonkeyStates -> MonkeyStates
runRound monkeys maybeShrinkWorry s0 = foldl' (runMonkeyTurn monkeys maybeShrinkWorry) s0 [0..nMonkeys-1]
  where
    nMonkeys = fromIntegral $ Map.size s0

-- |~) _  __|_  ~|~  _
-- |~ (_||  |    |VV(_)
--
-- >>> part2 . parseInput $ exampleInput
part2 ∷ [Monkey] → Int
part2 = common id 10000


-- |\/| _ . _
-- |  |(_||| |
--
main ∷ IO ()
main = do
  -- other testing here
  -- putText . prettyMap . initMonkeys . parseInput $ exampleInput
  -- putText . prettyMap . runRound
  --   $ initMonkeys $ parseInput exampleInput
  aocSinglePartMain "inputs/11.txt" exampleInput parseInput part1
  -- aocMain "inputs/11.txt" Solution { parse=parseInput, part1=part1, part2=part2 }


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

