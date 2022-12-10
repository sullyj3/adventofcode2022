-- designed to be imported unqualified
module AOC.Parse
  ( module Text.Megaparsec
  , decimal
  , Parser
  , unsafeParse
  ) where

import           Text.Megaparsec            hiding (count, State)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

unsafeParse ∷ (VisualStream stream, TraversableStream stream, ShowErrorComponent err) 
            ⇒ Parsec err stream a 
            → stream 
            → a
unsafeParse p t = case parse p "unsafeParse" t of
  Left  e → error $ "parsing with unsafeParse failed:\n" <> toText (errorBundlePretty e)
  Right a → a
