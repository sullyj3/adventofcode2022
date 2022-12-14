{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where

import           Data.Char       (digitToInt, intToDigit)
import           Data.List.Extra (anySame)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           Numeric
import           Optics.Core     (imap)
import qualified Relude.Unsafe   as Unsafe

both ∷ Bifunctor f ⇒ (a → b) → f a a → f b b
both = join bimap

parsePair ∷ (Text → a) → Text → Text → (a, a)
parsePair parseElems sep = both parseElems . twoListToPair . T.splitOn sep

parsePair2 ∷ (Text → b) → (Text → d) → Text → Text → (b, d)
parsePair2 parseLeft parseRight sep =
  bimap parseLeft parseRight . twoListToPair . T.splitOn sep

unreachable ∷ a
unreachable = error "Unreachable reached!"

twoListToPair ∷ [a] → (a,a)
twoListToPair [a,b] = (a,b)
twoListToPair l = error $ "This list has " <> show (length l) <> " elements!"

intList ∷ Text → Maybe [Int]
intList = traverse (readMaybe . toString) . lines

-- >>> parseBinary "1010" ∷ Maybe Word8
-- Just 10
-- >>> parseBinary "11111" ∷ Maybe Word8
-- Just 31
parseBinary ∷ Num a ⇒ Text → Maybe a
parseBinary t =
  case readInt 2 (`elem` binDigits) digitToInt (toString t) of
    [(n,_)] → Just n
    _       → Nothing
  where binDigits ∷ [Char]
        binDigits = ['0', '1']

-- >>> binaryLines "101\n001"
-- Just [5,1]
binaryLines ∷ Text → Maybe [Word32]
binaryLines = traverse parseBinary . lines

showBin ∷ (Integral a, Show a) ⇒ a → Text
showBin n = toText $ showIntAtBase 2 intToDigit n ""

tShow ∷ Show a ⇒ a → Text
tShow = toText @String . show

tReadMaybe ∷ Read a ⇒ Text → Maybe a
tReadMaybe = readMaybe . toString

tRead ∷ Read a ⇒ Text → a
tRead = Unsafe.read . toString

showSolutions ∷ (Show a, Show b) ⇒ a → b → Text
showSolutions p1 p2 =
  unlines ["Part 1: " <> tShow p1, "Part 2: " <> tShow p2]

-- iteratively pare down a list of candidates, returning the final candidate,
-- if it exists
elimination ∷ Monad m ⇒ ([a] → m [a]) → [a] → m (Maybe a)
elimination eliminateFrom = fix \loop -> \case
  [] → pure Nothing
  [x] → pure (Just x)
  remaining → do
    remaining' ← eliminateFrom remaining
    loop remaining'

-- assume indices are sorted
selectIndices ∷ [Int] → [a] → [a]
selectIndices = go 0
  where go _ [] _ = []
        go i (idx:idxs) (x:xs)
          | i == idx  = x : go (i+1) idxs xs
          | otherwise = go (i+1) (idx:idxs) xs
        go _ _ [] = []

selectIndices1 ∷ [Int] → [a] → [a]
selectIndices1 = selectIndices . map (subtract 1)

count ∷ (a → Bool) → [a] → Int
count p = length . filter p

slidingWindow ∷ Int → [a] → [[a]]
slidingWindow n = transpose' . take n . tails
  where transpose' = getZipList . traverse ZipList

allDistinct ∷ Eq a ⇒ [a] → Bool
allDistinct = not . anySame

prettyMap ∷ (Show k, Show v) ⇒ Map k v → Text
prettyMap = unlines . map (\(k,v) -> tShow k <> " -> " <> tShow v) . Map.toList

zipA ∷ Applicative f ⇒ f a → f b → f (a,b)
zipA = liftA2 (,)

countUniq ∷ Ord a ⇒ [a] → Int
countUniq = Set.size . Set.fromList

-- Compose a unary function with a binary one
-- Known as "Atop" in BQN
--   (.) . (.) cd abc
-- = ((cd .) .) abc
-- = ((cd .) . abc

--   (((cd .) . abc) a b
-- = ((cd .) abc a) b
-- = (cd . abc a) b
-- = cd (abc a b)
(.:) ∷ (c → d) → (a → b → c) → a → b → d
(.:) = (.) . (.)

data CardinalDir = U | D | L | R deriving (Eq, Show)

imap1 ∷ (Int → a → b) → [a] → [b]
imap1 f = imap (f . (+1))

divides ∷ Integral a ⇒ a → a → Bool
divides divisor dividend = dividend `mod` divisor == 0

nTimes :: Int -> (b -> b) -> b -> b
nTimes n f = go n
  where go 0 = id
        go i = f . go (i-1)
