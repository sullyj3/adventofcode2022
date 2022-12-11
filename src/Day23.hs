{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day23 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import PyF ( str )


-- |~) _  _ _. _  _                 
-- |~ (_|| _\|| |(_|                
--                _|                
-- >>> parseInput $ exampleInput
parseInput :: Text -> [()]
parseInput = unsafeParse $ linesOf $ pure ()


-- |~) _  __|_  /~\ _  _            
-- |~ (_||  |   \_/| |(/_           
--
-- >>> part1 . parseInput $ exampleInput
part1 :: a -> a
part1 = id


-- |~) _  __|_  ~|~  _              
-- |~ (_||  |    |VV(_)             
--
-- >>> part2 . parseInput $ exampleInput
part2 :: a -> a
part2 = id


-- |\/| _ . _                       
-- |  |(_||| |                      
--
main ∷ IO ()
main = do
  -- other testing here

  aocSinglePartMain "inputs/23.txt" exampleInput parseInput part1
  -- aocMain "inputs/23.txt" Solution { parse=parseInput, part1=part1, part2=part2 }


-- (~   _  _ _  _ | _   . _  _   _|_
-- (_><(_|| | ||_)|(/_  || ||_)|_|| 
--             |            |
exampleInput :: Text
exampleInput = toText @String [str||]

