{-# LANGUAGE NoFieldSelectors #-}
module Day07 (main) where

import           AOC
import           AOC.Parse            hiding (State)
import           Data.List            (minimum)
import qualified Data.List            as List
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T
import           Prelude              hiding (many, some)
import           PyF
import           Text.Megaparsec.Char (alphaNumChar, newline, string)


data CommandAndOutput = Cd Text | CdUp | Ls [LsEntry]
  deriving (Show, Eq, Ord)

data LsEntry = F { name :: Text, size :: Int}
             | D { name :: Text }
  deriving (Show, Eq, Ord)

type Session = [CommandAndOutput]

-------------
-- Parsing --
-------------
parseInput ∷ Text → FileTree
parseInput input = fileTree where
  session = unsafeParse (some commandAndOutputP) input
  fileTree = unsafeParse fileTreeP session

commandAndOutputP ∷ Parser CommandAndOutput
commandAndOutputP = do
  _ <- string "$ "
  try cdUpP <|>
    try cdP <|>
    try lsP
  where
    cdUpP ∷ Parser CommandAndOutput
    cdUpP = lineOf $ CdUp <$ string "cd .."

    cdP ∷ Parser CommandAndOutput
    cdP = lineOf $ Cd <$> (string "cd " *> fileNameP)

    lsP ∷ Parser CommandAndOutput
    lsP = Ls <$> (lineOf (string "ls") *> some lsEntryP)

    lsEntryP ∷ Parser LsEntry
    lsEntryP = lineOf $ try lsFileP <|> lsDirP

    lsFileP ∷ Parser LsEntry
    lsFileP = liftA2 (flip F) decimal (single ' ' *> fileNameP)

    lsDirP ∷ Parser LsEntry
    lsDirP = string "dir " *> (D <$> fileNameP)

    fileNameP ∷ Parser Text
    fileNameP = T.pack <$> some (alphaNumChar <|> single '.' <|> single '/')

lineOf ∷ Parser a → Parser a
lineOf p = p <* newline

data FileTree = FTDir Text [FileTree]
               | FTFile Text Int
  deriving Show

-- now parse the stream of CommandAndOutput into a FileTree

type ShellParser = Parsec Void Session

-- These instances don't do anything useful, but they're here to convince 
-- megaparsec to parse Sessions
instance VisualStream Session where
  showTokens :: Proxy Session → NonEmpty (Token Session) → String
  showTokens _ = List.unwords . NE.toList . fmap show

instance TraversableStream Session where
  reachOffset :: Int → PosState s → (Maybe String, PosState s)
  reachOffset _ s = (Nothing, s)

fileTreeP ∷ ShellParser FileTree
fileTreeP = dirP

dirP ∷ ShellParser FileTree
dirP = do
  Cd dirname <- satisfy isCd
  Ls entries <- satisfy isLs
  childDirs <- many dirP
  try (void $ satisfy isCdUp) <|> eof

  let children ∷ [FileTree]
      children = mapMaybe (\case (D _)         -> Nothing
                                 (F name size) -> Just $ FTFile name size)
                          entries
                <> childDirs

  pure $ FTDir dirname children
  where
    isCd (Cd _) = True
    isCd _      = False

    isLs (Ls _) = True
    isLs _      = False

    isCdUp CdUp = True
    isCdUp _    = False


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
