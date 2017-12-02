module Day2 where

import           Data.Char

numbers :: IO [[Int]]
numbers = do
  inp <- readFile "data/day2.txt"
  return $ sanitise inp

sanitise :: String -> [[Int]]
sanitise = ((fmap . fmap) read) . (fmap words) . lines

diff :: [Int] -> Int
diff row = (maximum row) - (minimum row)

divisible :: [Int] -> Int
divisible row =
  head [(div x y) | x <- row, y <- row, x /= y, rem x y == 0]

partOne :: IO Int
partOne = solve diff

partTwo :: IO Int
partTwo = solve divisible

solve :: ([Int] -> Int) -> IO Int
solve mapWith = do
  n <- numbers
  return $ sum $ fmap mapWith n

