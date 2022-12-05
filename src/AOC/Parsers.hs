-- designed to be imported qualified
module AOC.Parsers
  ( commaSeparatedInts
  , pairOf
  , numPair
  , linesOf
  , numLine
  ) where

import           AOC.Parse
import           Text.Megaparsec.Char
import Prelude hiding (many, some)
import Data.Char (isDigit)

commaSeparatedInts ∷ Parser [Int]
commaSeparatedInts = decimal `sepBy` single ','

pairOf ∷ Parser a → Text → Parser (a, a)
pairOf p sep = liftA2 (,) p (string sep *> p)

numPair ∷ Num a ⇒ Text → Parser (a, a)
numPair = pairOf decimal

linesOf ∷ Parser a → Parser [a]
linesOf p = p `sepEndBy` newline

-- treats all non-digit characters as separators
numLine :: Num a => Parser [a]
numLine = many nonDigit *> decimal `sepEndBy1` some nonDigit
  where
    nonDigit = satisfy $ \c -> not (isDigit c) && c /= '\n'

