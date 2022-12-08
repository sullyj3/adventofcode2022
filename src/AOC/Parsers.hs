-- designed to be imported qualified
module AOC.Parsers
  ( commaSeparatedInts
  , pairOf
  , pairOfBoth
  , numPair
  , linesOf
  , numLine
  ) where

import           AOC.Parse
import           Text.Megaparsec.Char
import Prelude hiding (many, some)
import Data.Char (isDigit)
import Utils (zipA)

commaSeparatedInts ∷ Parser [Int]
commaSeparatedInts = decimal `sepBy` single ','

pairOf ∷ Parser a → Text → Parser (a, a)
pairOf p sep = p `zipA` (string sep *> p)

pairOfBoth ∷ Parser a → Parser b → Text → Parser (a, b)
pairOfBoth p1 p2 sep = p1 `zipA` (string sep *> p2)

numPair ∷ Num a ⇒ Text → Parser (a, a)
numPair = pairOf decimal

linesOf ∷ Parser a → Parser [a]
linesOf p = p `sepEndBy` newline

-- treats all non-digit characters as separators
numLine :: Num a => Parser [a]
numLine = many nonDigit *> decimal `sepEndBy1` some nonDigit
  where
    nonDigit = satisfy $ \c -> not (isDigit c) && c /= '\n'

