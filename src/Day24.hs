{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day24 (main) where

import           AOC
import           AOC.Parse
import           AOC.Parsers
import PyF ( str )

-- ╔═╗┌─┐┬─┐┌─┐┬┌┐┌┌─┐                  
-- ╠═╝├─┤├┬┘└─┐│││││ ┬                  
-- ╩  ┴ ┴┴└─└─┘┴┘└┘└─┘                  

-- >>> parseInput $ exampleInput
parseInput :: a -> a
parseInput = id
-- parseInput = unsafeParse numLine
-- parseInput = toString

-- ╔═╗┌─┐┬─┐┌┬┐  ╔═╗┌┐┌┌─┐              
-- ╠═╝├─┤├┬┘ │   ║ ║│││├┤               
-- ╩  ┴ ┴┴└─ ┴   ╚═╝┘└┘└─┘              
-- >>> part1 . parseInput $ exampleInput
part1 :: a -> a
part1 = id

-- ╔═╗┌─┐┬─┐┌┬┐  ╔╦╗┬ ┬┌─┐              
-- ╠═╝├─┤├┬┘ │    ║ ││││ │              
-- ╩  ┴ ┴┴└─ ┴    ╩ └┴┘└─┘              

-- >>> part2 . parseInput $ exampleInput
part2 :: a -> a
part2 = id

main ∷ IO ()
main = do
  -- other testing here

  aocSinglePartMain "inputs/24.txt" exampleInput parseInput part1

  -- aocMain "inputs/24.txt" Solution { parse=parseInput, part1=part1, part2=part2 }

-- ╔═╗─┐ ┬┌─┐┌┬┐┌─┐┬  ┌─┐  ┬┌┐┌┌─┐┬ ┬┌┬┐
-- ║╣ ┌┴┬┘├─┤│││├─┘│  ├┤   ││││├─┘│ │ │ 
-- ╚═╝┴ └─┴ ┴┴ ┴┴  ┴─┘└─┘  ┴┘└┘┴  └─┘ ┴
exampleInput :: Text
exampleInput = toText @String [str||]

