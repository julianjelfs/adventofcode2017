module Day3 where

import           Data.Bifunctor

--there's definitely a pure maths way to do this but I can't
--17  16  15  14  13
--18   5   4   3  12
--19   6   1   2  11
--20   7   8   9  10
--21  22  23---> ...

--continue in the same direction until either coordinate is beyond the range you have visited before

data Direction
  = West
  | East
  | North
  | South
  deriving (Show)

type Coordinates = (Int, Int)

data Range =
  Range (Int, Int) (Int, Int)
  deriving Show

type Value = Int

data MoveState =
  MoveState Range
            Coordinates
            Direction
            Value
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

findCoordinate :: Value -> MoveState -> MoveState
findCoordinate target (MoveState r c d v)
  | target == v = MoveState r c d v
  | otherwise =
    let
      d1 = if shouldChangeDirection c r then changeDirection d else d
      r1 = updateRange c r
    in
      findCoordinate target (MoveState r1 (move d1 c) d1 (v+1))

solve :: Value -> Int
solve target =
  let
    (MoveState r c d v) = findCoordinate target (MoveState (Range (0,0) (0,0)) (0,0) East 1)
  in
    manhattanDistance (0,0) c
