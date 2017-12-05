module Day5 where

import qualified Data.Vector as V

jump :: (Int -> Int) -> Int -> V.Vector Int -> (Int, V.Vector Int)
jump incrementor i v =
  case (v V.!? i) of
    Nothing -> (-1, v)
    Just x ->
        (i + x, v V.// [(i, (incrementor x))])

solve :: (Int -> Int) -> (Int, Int, V.Vector Int) -> Int
solve incrementor (n, i, v) =
    if i == -1 then
        n - 1
    else
        let
            (i1, v1) = jump incrementor i v
        in solve incrementor (n + 1, i1, v1)

partOne :: IO Int
partOne = do
  inp <- readFile "data/day5.txt"
  return $ solve succ (0, 0, (V.fromList (fmap read $ lines inp)))

partTwo :: IO Int
partTwo = do
  inp <- readFile "data/day5.txt"
  return $ solve (\n -> if n >= 3 then n - 1 else n + 1) (0, 0, (V.fromList (fmap read $ lines inp)))

test =
  solve (\n -> if n >= 3 then n - 1 else n + 1) (0, 0, (V.fromList [0,3,0,1,-3]))


