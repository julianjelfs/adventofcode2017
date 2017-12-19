module Day19 where

import qualified Data.Vector as V

newtype Maze = Maze (V.Vector (V.Vector Char))

data Direction
  = North
  | South
  | East
  | West
  deriving Show

instance Show Maze where
  show (Maze v) =
    V.foldl' showLine "" v
    where showLine str l = (V.foldl' showCell str l) ++ ['\n']
          showCell str c = str ++ [c]

parse = do
  inp <- readFile "data/day19.txt"
  let rows = fmap V.fromList $ fmap V.fromList lines inp
      maze = Maze rows
  return $ maze

partOne = do
  (Maze m) <- parse
  let start = V.elemIndex '|' $ m V.! 0
  return $ fmap reverse $ fmap snd $ fmap (\x -> walkMaze South m [] [] (x,0)) start

partTwo = do
  (Maze m) <- parse
  let start = V.elemIndex '|' $ m V.! 0
  return $ fmap length $ fmap fst $ fmap (\x -> walkMaze South m [] [] (x,0)) start

walkMaze dir maze path letters (x, y) =
    case getValue maze (x,y) of
        Nothing -> (path, letters)
        Just ' ' -> (path, letters)
        Just '+' ->
            let newDir = changeDirection maze dir (x, y)
            in walkMaze newDir maze ((x,y):path) letters (nextCoord newDir (x, y))
        Just c -> walkMaze dir maze ((x,y):path) (captureLetter c letters) (nextCoord dir (x, y))

captureLetter c letters
    | elem c ['A'..'Z'] = c:letters
    | otherwise = letters

changeDirection maze dir (x,y) =
    let candidates =
            case dir of
                North -> [East, West]
                South -> [East, West]
                East -> [North, South]
                West -> [North, South]
    in
        case filter (valid maze (x,y)) candidates of
            [] -> dir
            (h:_) -> h

valid maze (x,y) dir =
    case getValue maze $ nextCoord dir (x,y) of
        Nothing -> False
        Just ' ' -> False
        _ -> True

getValue maze (x,y) = do
    r <- maze V.!? y
    c <- r V.!? x
    return c

nextCoord North (x, y) = (x, y-1)
nextCoord South (x, y) = (x, y+1)
nextCoord East (x, y) = (x+1, y)
nextCoord West (x, y) = (x-1, y)


