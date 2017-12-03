module Day3 where

import           Data.Bifunctor
import qualified Data.Map       as Map
import           Data.Maybe

data Direction
  = West
  | East
  | North
  | South
  deriving (Show)

type Coordinates = (Int, Int)

data Range =
  Range (Int, Int)
        (Int, Int)
  deriving (Show)

type Value = Int

data MoveState =
  MoveState Range
            Coordinates
            Direction
            Value
            (Map.Map Coordinates Value)
  deriving (Show)

dec = \n -> n - 1

inc = \n -> n + 1

move :: Direction -> Coordinates -> Coordinates
move West c  = bimap dec id c
move East c  = bimap inc id c
move North c = bimap id dec c
move South c = bimap id inc c

manhattanDistance :: Coordinates -> Coordinates -> Int
manhattanDistance (x1, y1) (x2, y2) = (abs (x2 - x1)) + (abs (y2 - y1))

changeDirection :: Direction -> Direction
changeDirection West  = South
changeDirection South = East
changeDirection East  = North
changeDirection North = West

shouldChangeDirection :: Coordinates -> Range -> Bool
shouldChangeDirection (x, y) (Range (minX, minY) (maxX, maxY)) =
  x > maxX || x < minX || y > maxY || y < minY

updateRange :: Coordinates -> Range -> Range
updateRange (x, y) (Range (minX, minY) (maxX, maxY)) =
  Range (min minX x, min minY y) (max maxX x, max maxY y)

sumOfSurroundingCells :: Coordinates -> Map.Map Coordinates Value -> Int
sumOfSurroundingCells c s =
  sum $ catMaybes $ (fmap (\c1 -> Map.lookup c1 s) (surroundingCells c))

surroundingCells :: Coordinates -> [Coordinates]
surroundingCells (x, y) =
  [ (x + 1, y)
  , (x + 1, y + 1)
  , (x + 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x - 1, y - 1)
  , (x, y + 1)
  , (x, y - 1)
  ]

findCoordinate :: Value -> MoveState -> MoveState
findCoordinate target (MoveState r c d v s)
  | v >= target = MoveState r c d v s
  | otherwise =
    let d1 =
          if shouldChangeDirection c r
            then changeDirection d
            else d
        r1 = updateRange c r
        c1 = move d1 c
        v1 = sumOfSurroundingCells c1 s
        s1 = Map.insert c1 v1 s
    in findCoordinate target (MoveState r1 c1 d1 v1 s1)

solve :: Value -> Int
solve target =
  let (MoveState r c d v s) = findCoordinate target initialState
  in v

initialState =
  (MoveState (Range (0, 0) (0, 0)) (0, 0) East 1 (Map.insert (0, 0) 1 Map.empty))
