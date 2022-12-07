{-# LANGUAGE NoFieldSelectors #-}
module Day07 (main) where

import           AOC
import           AOC.Parse            hiding (State)
import           Data.List            (minimum)
import qualified Data.Text            as T
import           Prelude              hiding (many, some)
import           Text.Megaparsec.Char (alphaNumChar, newline, string)


-------------
-- Parsing --
-------------
parseInput ∷ Text → FileTree
parseInput = unsafeParse fileTreeP

lineOf ∷ Parser a → Parser a
lineOf p = p <* newline

data LeafTree a = Branch [LeafTree a]
                | Leaf a
  deriving (Show, Eq, Functor, Foldable, Traversable)

type FileTree = LeafTree Int

fileTreeP ∷ Parser FileTree
fileTreeP = dirP

dirP ∷ Parser FileTree
dirP = do
  _ <- cdP
  entries <- lsP
  childDirs <- many (try dirP)
  try cdUpP <|> eof

  let children ∷ [FileTree]
      children = catMaybes entries <> childDirs

  pure $ Branch children
  where
    cdP = lineOf $ string "$ cd " *> fileNameP
    lsP = lineOf (string "$ ls") *> many lsEntryP
    lsEntryP = lineOf $ try lsFileP <|> lsDirP
    lsFileP = Just <$> (Leaf <$> decimal) <* (single ' ' *> fileNameP)
    lsDirP = Nothing <$ (string "dir " *> fileNameP)
    cdUpP = void $ lineOf $ string "$ cd .."
    fileNameP = do 
      name <- T.pack <$> some (alphaNumChar <|> single '.' <|> single '/')
      guard (name /= "..")
      pure name

---------------
-- Solutions --
---------------

directories ∷ FileTree → [FileTree]
directories (Leaf _) = []
directories d@(Branch children) = d : concatMap directories children

ftSize ∷ FileTree → Int
ftSize = sum

dirSizes ∷ FileTree → [Int]
dirSizes = map ftSize . directories

part1 ∷ FileTree → Int
part1 = sum . filter (<=100000) . dirSizes

part2 ∷ FileTree → Int
part2 fileTree = minimum . filter (>=spaceToFree) . dirSizes $ fileTree
  where
    spaceToFree = ftSize fileTree - 70000000 + 30000000

main ∷ IO ()
main =
  aocMain "inputs/07.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
