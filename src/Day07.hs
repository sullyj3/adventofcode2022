{-# LANGUAGE NoFieldSelectors #-}
module Day07 (main) where

import           AOC
import           AOC.Parse            hiding (State)
import           Data.List            (foldl1', minimum)
import qualified Data.Text            as T
import           Prelude              hiding (many, some)
import           PyF
import           Text.Megaparsec.Char (alphaNumChar, newline, string)
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List


data File = File { fsize :: Int }
          | Dir  { dircontents :: [Text] } -- list of absolute paths
  deriving (Show)

data CommandAndOutput = Cd Text | CdUp | Ls [LsEntry]
  deriving (Show, Eq, Ord)

data LsEntry = F { name :: Text, size :: Int}
             | D { name :: Text }
  deriving (Show, Eq, Ord)

type Session = [CommandAndOutput]

instance VisualStream Session where
  showTokens :: Proxy Session -> NonEmpty (Token Session) -> String
  showTokens p = List.unwords . NE.toList . fmap show

instance TraversableStream Session where
  reachOffset :: Int -> PosState s -> (Maybe String, PosState s)
  reachOffset i s = (Nothing, s)

-------------
-- Parsing --
-------------
parseInput ∷ Text → FileTree2
parseInput input = fileTree where
  session = unsafeParse (some commandAndOutputP) input
  fileTree = unsafeParse fileTreeP session

commandAndOutputP ∷ Parser CommandAndOutput
commandAndOutputP = do
  _ <- string "$ "
  try cdUpP <|>
    try cdP <|>
    try lsP

cdP ∷ Parser CommandAndOutput
cdP = lineOf $ Cd <$> (string "cd " *> fileNameP)

cdUpP ∷ Parser CommandAndOutput
cdUpP = lineOf $ CdUp <$ string "cd .."

fileNameP ∷ Parser Text
fileNameP = T.pack <$> some (alphaNumChar <|> single '.' <|> single '/')

lsP ∷ Parser CommandAndOutput
lsP = Ls <$> (lineOf (string "ls") *> some lsEntryP)

lsEntryP ∷ Parser LsEntry
lsEntryP = lineOf $ try lsFileP <|> lsDirP

lsDirP ∷ Parser LsEntry
lsDirP = string "dir " *> (D <$> fileNameP)

lsFileP ∷ Parser LsEntry
lsFileP = liftA2 (flip F) decimal (single ' ' *> fileNameP)

lineOf ∷ Parser a → Parser a
lineOf p = p <* newline

data FileTree2 = FTDir Text [FileTree2]
               | FTFile Text Int
  deriving Show

-- now parse the stream of commandAndOutputP into a FileTree2

type ShellParser = Parsec Void Session

fileTreeP ∷ ShellParser FileTree2
fileTreeP = dirP

dirP ∷ ShellParser FileTree2
dirP = do
  Cd dirname <- satisfy isCd
  Ls entries <- satisfy isLs
  childDirs <- many dirP
  try (void $ satisfy isCdUp) <|> eof

  let children ∷ [FileTree2]
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

nodes ∷ FileTree2 → [FileTree2]
nodes t@(FTDir _ ts) = t : concatMap nodes ts
nodes t@(FTFile _ _) = [t]

directories ∷ FileTree2 → [FileTree2]
directories = filter isDir . nodes where
  isDir (FTDir _ _) = True
  isDir _           = False

ftSize :: FileTree2 -> Int
ftSize (FTDir _ contents) = sum (map ftSize contents)
ftSize (FTFile _ size) = size

part1 ∷ FileTree2 → Int
part1 = sum . filter (<=100000) . map ftSize . directories

part2 ∷ FileTree2 → Int
part2 fileTree = dirSizeToDelete
  where
    totalUsedSpace = ftSize fileTree
    sizes = map ftSize . directories $ fileTree
    targetUsedSpace = 70000000 - 30000000
    spaceToFree = totalUsedSpace - targetUsedSpace
    dirSizeToDelete = minimum . filter (>=spaceToFree) $ sizes

main ∷ IO ()
main = do
  -- let input = parseInput exampleInput
  -- print $ part1 input
  -- other testing here
  -- aocSinglePartMain "inputs/07.txt" exampleInput parseInput part1

  aocMain "inputs/07.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

exampleInput ∷ Text
exampleInput = toText @String [str|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|]

