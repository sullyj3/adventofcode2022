-- designed to be imported unqualified
module AOC.Parse
  ( module Text.Megaparsec
  , decimal
  , Parser
  , unsafeParse
  ) where

import qualified Relude.Unsafe              as Unsafe
import           Text.Megaparsec            hiding (count, parse)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

unsafeParse ∷ Parser a → Text → a
unsafeParse p = Unsafe.fromJust . parseMaybe p
