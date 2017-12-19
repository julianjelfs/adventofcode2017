module Day19 where

import Data.List

data Direction
  = North
  | South
  | East
  | West
  deriving (Show)

parse = do
  inp <- readFile "data/day19.txt"
  return $ lines inp

solve = do
  m <- parse
  let start = elemIndex '|' $ m !! 0
      res = fmap (\x -> walkMaze South m [] [] (x, 0)) start
  return $
    ( fmap reverse $ fmap snd res
    , fmap length $ fmap fst res )

walkMaze dir maze path letters (x, y) =
  case getValue maze (x, y) of
    Nothing -> (path, letters)
    Just ' ' -> (path, letters)
    Just '+' ->
      let newDir = changeDirection maze dir (x, y)
      in walkMaze newDir maze ((x, y) : path) letters (nextCoord newDir (x, y))
    Just c ->
      walkMaze
        dir
        maze
        ((x, y) : path)
        (captureLetter c letters)
        (nextCoord dir (x, y))

captureLetter c letters
  | elem c ['A' .. 'Z'] = c : letters
  | otherwise = letters

changeDirection maze dir (x, y) =
  let candidates =
        case dir of
          North -> [East, West]
          South -> [East, West]
          East -> [North, South]
          West -> [North, South]
  in case filter (valid maze (x, y)) candidates of
       [] -> dir
       (h:_) -> h

valid maze (x, y) dir =
  case getValue maze $ nextCoord dir (x, y) of
    Nothing -> False
    Just ' ' -> False
    _ -> True

getValue maze (x, y) =
  if y >= 0 && y < length maze
    then let r = maze !! y
         in if x >= 0 && x < length r
              then Just $ r !! x
              else Nothing
    else Nothing

nextCoord North (x, y) = (x, y - 1)
nextCoord South (x, y) = (x, y + 1)
nextCoord East (x, y) = (x + 1, y)
nextCoord West (x, y) = (x - 1, y)
