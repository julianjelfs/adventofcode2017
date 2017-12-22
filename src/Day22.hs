module Day22 where

import qualified Data.Map.Strict as M

data Direction
  = U | D | L | R
  deriving Show

data NodeState
  = Clean | Weakened | Infected | Flagged
  deriving Show

parse = do
  inp <- readFile "data/day22.txt"
  return $
    M.fromList [ ((x,y), z)
               | (y, l) <- zip [-12..] $ lines inp
               , (x, c) <- zip [-12..] l
               , let z = if c == '#' then Infected else Clean ]

partTwo = do
  grid <- parse
  return $ burst 0 0 (0,0) U grid

burst infections 10000000 _ _ _ = infections
burst infections iterations pos@(x,y) dir grid =
  case M.lookup pos grid of
    Just Weakened ->
      burst (infections + 1) (iterations+1) (move dir pos) dir (M.insert pos Infected grid)
    Just Infected ->
      let d = turnRight dir
      in burst infections (iterations+1) (move d pos) d (M.insert pos Flagged grid)
    Just Flagged ->
      let d = reverseDir dir
      in burst infections (iterations+1) (move d pos) d (M.insert pos Clean grid)
    _ ->  --clean
      let d = turnLeft dir
      in burst infections (iterations+1) (move d pos) d (M.insert pos Weakened grid)

turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

reverseDir U = D
reverseDir L = R
reverseDir D = U
reverseDir R = L

move U (x,y) = (x,y-1)
move R (x,y) = (x+1,y)
move D (x,y) = (x,y+1)
move L (x,y) = (x-1,y)
