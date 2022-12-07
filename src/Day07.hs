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

data FileTree = FTDir Text [FileTree]
              | FTFile Text Int
  deriving Show

fileTreeP ∷ Parser FileTree
fileTreeP = dirP

dirP ∷ Parser FileTree
dirP = do
  dirname <- cdP
  entries <- lsP
  childDirs <- many (try dirP)
  try cdUpP <|> eof

  let children ∷ [FileTree]
      children = catMaybes entries <> childDirs

  pure $ FTDir dirname children
  where
    cdP = lineOf $ string "$ cd " *> fileNameP
    lsP = lineOf (string "$ ls") *> many lsEntryP
    lsEntryP = lineOf $ try lsFileP <|> lsDirP
    lsFileP = Just <$> liftA2 (flip FTFile) decimal (single ' ' *> fileNameP)
    lsDirP = Nothing <$ (string "dir " *> fileNameP)
    cdUpP = void $ lineOf $ string "$ cd .."
    fileNameP = do 
      name <- T.pack <$> some (alphaNumChar <|> single '.' <|> single '/')
      guard (name /= "..")
      pure name

---------------
-- Solutions --
---------------

nodes ∷ FileTree → [FileTree]
nodes t@(FTDir _ ts) = t : concatMap nodes ts
nodes t@(FTFile _ _) = [t]

directories ∷ FileTree → [FileTree]
directories = filter isDir . nodes where
  isDir (FTDir _ _) = True
  isDir _           = False

ftSize ∷ FileTree → Int
ftSize (FTDir _ contents) = sum (map ftSize contents)
ftSize (FTFile _ size)    = size

part1 ∷ FileTree → Int
part1 = sum . filter (<=100000) . map ftSize . directories

part2 ∷ FileTree → Int
part2 fileTree = minimum . filter (>=spaceToFree) $ sizes
  where
    totalUsedSpace = ftSize fileTree
    sizes = map ftSize . directories $ fileTree
    targetUsedSpace = 70000000 - 30000000
    spaceToFree = totalUsedSpace - targetUsedSpace

main ∷ IO ()
main =
  aocMain "inputs/07.txt" Solution { parse=parseInput, part1=part1, part2=part2 }
