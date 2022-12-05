-- designed to be imported qualified
module AOC.Parsers
  ( commaSeparatedInts
  , pairOf
  , numPair
  , linesOf
  ) where

import           AOC.Parse
import           Text.Megaparsec.Char

commaSeparatedInts ∷ Parser [Int]
commaSeparatedInts = decimal `sepBy` single ','

pairOf ∷ Parser a → Text → Parser (a, a)
pairOf p sep = liftA2 (,) p (string sep *> p)

numPair ∷ Num a ⇒ Text → Parser (a, a)
numPair = pairOf decimal

linesOf ∷ Parser a → Parser [a]
linesOf p = p `sepEndBy` newline
