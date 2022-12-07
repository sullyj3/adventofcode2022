{-# LANGUAGE NoFieldSelectors #-}
module Day07 (main) where

import           AOC
import           AOC.Parse hiding (State)
import           AOC.Parsers
import qualified Data.Text   as T
import           PyF
import           Utils       (tRead, prettyMap)
import Prelude hiding (some)
import Text.Megaparsec.Char (string, newline, alphaNumChar)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Data.List (foldl1', minimum)


-- map of fully qualified paths to their contents
type FileTree = Map Text File

data File = File { fsize :: Int }
          | Dir  { dircontents :: [Text] } -- list of absolute paths
  deriving (Show)

data CommandAndOutput = Cd Text | CdUp | Ls [LsEntry]
  deriving Show

data LsEntry = F { size :: Int, name :: Text}
             | D { name :: Text }
  deriving Show

type Session = [CommandAndOutput]


-------------
-- Parsing --
-------------
parseInput :: Text -> Session
parseInput = unsafeParse $ some commandAndOutputP

commandAndOutputP :: Parser CommandAndOutput
commandAndOutputP = do
  _ <- string "$ "
  try cdUpP <|>
    try cdP <|>
    try lsP

cdP :: Parser CommandAndOutput
cdP = lineOf $ Cd <$> (string "cd " *> fileNameP)

cdUpP :: Parser CommandAndOutput
cdUpP = lineOf $ CdUp <$ string "cd .." 

fileNameP :: Parser Text
fileNameP = T.pack <$> some (alphaNumChar <|> single '.' <|> single '/')

lsP :: Parser CommandAndOutput
lsP = Ls <$> (lineOf (string "ls") *> some lsEntryP)

lsEntryP :: Parser LsEntry
lsEntryP = lineOf $ try lsFileP <|> lsDirP

lsDirP :: Parser LsEntry
lsDirP = string "dir " *> (D <$> fileNameP)

lsFileP :: Parser LsEntry
lsFileP = liftA2 F decimal (single ' ' *> fileNameP)

lineOf :: Parser a -> Parser a
lineOf p = p <* newline


---------------
-- Solutions --
---------------

type BreadCrumbs = [Text]

buildTree :: Session -> FileTree
buildTree session = fst $ execState (loop session) (Map.singleton "/" (Dir []), [])
  where
    loop :: Session -> State (FileTree, BreadCrumbs) ()
    loop = traverse_ $ \case
      Ls lsEntries -> insertFiles lsEntries
      Cd dir -> modify $ second (dir:)
      CdUp -> modify $ second (drop 1)

    toFile :: LsEntry -> File
    toFile = \case
      F size _ -> File size
      D _      -> Dir []

    insertFiles :: [LsEntry] -> State (FileTree, BreadCrumbs) ()
    insertFiles lsEntries = do
      currentDirPath <- gets (fullyQualifiedPath . snd)
      -- traceM $ "inserting files in " <> show currentDirPath
      breadcrumbs <- gets snd
      -- traceM $ "breadcrumbs = " <> show breadcrumbs

      -- insert the children of the current directory into the tree, returning each of their paths
      -- TODO check map api for non for loop way of doing this
      absPaths <- for lsEntries $ \entry -> do
        let absPath = currentDirPath </> entry.name
        -- traceM $ "inserting " <> show absPath
        modify $ first $ Map.insert absPath (toFile entry)
        pure absPath

      -- set the contents of the current directory to the list of its children
      modify $ first $ Map.insert currentDirPath (Dir absPaths)

(</>) :: Text -> Text -> Text
"/" </> p2 = "/" <> p2
p1 </> p2 = p1 <> "/" <> p2

fullyQualifiedPath :: BreadCrumbs -> Text
fullyQualifiedPath crumbs = foldl1' (</>) (reverse crumbs)

recursiveDirSize :: FileTree -> Text -> Int
recursiveDirSize tree path = case Map.lookup path tree of
  Nothing -> error $ "no such path: " <> show path
  Just (File size) -> size
  Just (Dir contents) -> sum $ recursiveDirSize tree <$> contents

directories :: FileTree -> [Text]
directories = Map.keys . Map.filter isDir
  where
    isDir (Dir _) = True
    isDir _       = False

part1 :: Session -> Int
part1 session = sum . filter (<=100000) $ sizes
  where 
    tree = buildTree session
    dirPaths = directories tree
    sizes = map (recursiveDirSize tree) dirPaths

part2 session = dirSizeToDelete
  where
    tree = buildTree session
    totalUsedSpace = recursiveDirSize tree "/"
    dirPaths = directories tree
    sizes = map (recursiveDirSize tree) dirPaths

    targetUsedSpace = 70000000 - 30000000
    spaceToFree = totalUsedSpace - targetUsedSpace

    dirSizeToDelete = minimum . filter (>=spaceToFree) $ sizes

main ∷ IO ()
main = do
  -- let input = parseInput exampleInput
  -- print $ part1 input
  -- other testing here
  aocSinglePartMain "inputs/07.txt" exampleInput parseInput part2

  -- aocMain "inputs/07.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

exampleInput :: Text
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

