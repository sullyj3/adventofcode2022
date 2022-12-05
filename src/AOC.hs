{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module AOC where

import qualified Data.Text.IO as T
import Utils (both)
import Data.Biapplicative

data Solution i o = Solution 
  { parse ∷ Text → i
  , part1 ∷ i → o
  , part2 ∷ i → o
  }

aocMain ∷ Show o ⇒ FilePath → Solution i o → IO ()
aocMain inputPath sol =
  uncurry (*>)
    . both putStrLn
    . (<<*>>) (both (<>) ("part 1: ", "part 2: ")) 
    . both show 
    . (sol.part1 &&& sol.part2) 
    . sol.parse
    =<< T.readFile inputPath
