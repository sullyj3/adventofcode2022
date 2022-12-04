
module Utils where

import qualified Data.Text as T
import Data.Text (Text)
import Numeric
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word32)
import Text.Read (readMaybe)

intList ∷ Text → Maybe [Int]
intList = traverse (readMaybe . T.unpack) . T.lines

-- >>> parseBinary "1010" ∷ Maybe Word8
-- Just 10
-- >>> parseBinary "11111" ∷ Maybe Word8
-- Just 31
parseBinary ∷ (Eq a, Num a) ⇒ Text → Maybe a
parseBinary t =
  case readInt 2 (`elem` binDigits) digitToInt (T.unpack t) of
    [(n,_)] → Just n
    _ → Nothing
  where binDigits ∷ [Char]
        binDigits = ['0', '1']

-- >>> binaryLines "101\n001"
-- Just [5,1]
binaryLines ∷ Text → Maybe [Word32]
binaryLines = traverse parseBinary . T.lines

showBin ∷ (Integral a, Show a) ⇒ a → Text
showBin n = T.pack $ showIntAtBase 2 intToDigit n ""

tShow ∷ Show a ⇒ a → Text
tShow = T.pack . show

tReadMaybe ∷ Read a ⇒ Text → Maybe a
tReadMaybe = readMaybe . T.unpack

showSolutions ∷ (Show a, Show b) ⇒ a → b → Text
showSolutions p1 p2 =
  T.unlines ["Part 1: " <> tShow p1, "Part 2: " <> tShow p2]

-- iteratively pare down a list of candidates, returning the final candidate,
-- if it exists
elimination ∷ Monad m ⇒ ([a] → m [a]) → [a] → m (Maybe a)
elimination eliminateFrom = loop
  where
    loop = \case
      [] → pure Nothing
      [x] → pure (Just x)
      remaining → do
        remaining' <- eliminateFrom remaining
        loop remaining'

-- assume indices are sorted
selectIndices ∷ [Int] → [a] → [a]
selectIndices = go 0
  where go _ [] _ = []
        go i (idx:idxs) (x:xs)
          | i == idx  = x : go (i+1) idxs xs
          | otherwise = go (i+1) (idx:idxs) xs
        go _ _ [] = error "list doesn't contain all of the indices."
        

count ∷ (a → Bool) → [a] → Int
count p = length . filter p

