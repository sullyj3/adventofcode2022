{-# LANGUAGE TypeFamilies #-}
module Day07 (main) where

import           AOC
import           AOC.Parse                hiding (State)
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           Data.List                (minimum)
import qualified Data.Text                as T
import           Prelude                  hiding (many, some)
import           Text.Megaparsec.Char     (alphaNumChar, newline, string)

data LeafTree a = Branch [LeafTree a]
                | Leaf a
  deriving (Eq, Foldable, Functor, Show, Traversable)

type FileTree = LeafTree Int

makeBaseFunctor ''LeafTree

-------------
-- Parsing --
-------------
parseInput ∷ Text → FileTree
parseInput = unsafeParse fileTreeP

lineOf ∷ Parser a → Parser a
lineOf p = p <* newline


fileTreeP ∷ Parser FileTree
fileTreeP = dirP

dirP ∷ Parser FileTree
dirP = do
  cdP
  childFiles <- lsP
  childDirs <- many (try dirP)
  try cdUpP <|> eof
  pure $ Branch (childFiles <> childDirs)
  where
    cdP = void $ lineOf $ string "$ cd " *> fileNameP
    lsP = lineOf (string "$ ls") *> (catMaybes <$> many lsEntryP)
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

-- todo: is this a histo?
dirSizes ∷ FileTree → [Int]
dirSizes = map sum . directories
  where
    directories = para \case
      LeafF _                             -> []
      BranchF (unzip -> (subTrees, dirs)) -> Branch subTrees : concat dirs

part1 ∷ FileTree → Int
part1 = sum . filter (<=100000) . dirSizes

part2 ∷ FileTree → Int
part2 fileTree = minimum . filter (>=spaceToFree) . dirSizes $ fileTree
  where
    spaceToFree = sum fileTree - 70000000 + 30000000

main ∷ IO ()
main =
  aocMain "inputs/07.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
