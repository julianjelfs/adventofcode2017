module Day11 where

import           Data.List.Split

solve = do
  inp <- readFile "data/day11.txt"
  let path = (splitOn "," inp :: [String])
  return $
    foldl
      (\(c, d) m ->
         let pos = move m c
             d1 = distance pos
         in (pos, max d d1))
      ((0, 0, 0), 0)
      path

move "n" (x, y, z)  = (x, y + 1, z - 1)
move "ne" (x, y, z) = (x + 1, y, z - 1)
move "se" (x, y, z) = (x + 1, y - 1, z)
move "s" (x, y, z)  = (x, y - 1, z + 1)
move "sw" (x, y, z) = (x - 1, y, z + 1)
move "nw" (x, y, z) = (x - 1, y + 1, z)

distance (x, y, z) = maximum [x, y, z]
