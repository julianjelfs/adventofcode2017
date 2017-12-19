module Day19 where

import qualified Data.Vector as V

newtype Maze = Maze (V.Vector (V.Vector Char))

data Direction
  = North
  | South
  | East
  | West

instance Show Maze where
  show (Maze v) =
    V.foldl' showLine "" v
    where showLine str l = (V.foldl' showCell str l) ++ ['\n']
          showCell str c = str ++ [c]

parse = do
  inp <- readFile "data/day19test.txt"
  let rows = fmap V.fromList $ fmap V.fromList lines inp
      maze = Maze rows
  return $ maze

partOne = do
  (Maze m) <- parse
  let start = V.elemIndex '|' $ m V.! 0
  return $ fmap (walkMaze [] 0) start

walkMaze letters y x = undefined

nextCoord North (x, y) = (x, y-1)
nextCoord South (x, y) = (x, y+1)
nextCoord East (x, y) = (x+1, y)
nextCoord West (x, y) = (x-1, y)


