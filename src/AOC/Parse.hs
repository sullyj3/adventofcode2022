-- designed to be imported unqualified
module AOC.Parse
  ( module Text.Megaparsec
  , decimal
  , Parser
  , unsafeParse
  ) where

import           Text.Megaparsec            hiding (count)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

unsafeParse ∷ Parser a → Text → a
unsafeParse p t = case parse p "unsafeParse" t of
  Left  e → error $ "parsing with unsafeParse failed:\n" <> toText (errorBundlePretty e)
  Right a → a
