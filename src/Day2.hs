module Day2 where

import           Data.Char

numbers :: IO [[Int]]
numbers = do
  inp <- readFile "data/day2.txt"
  return $ sanitise $ lines inp

diff :: [Int] -> Int
diff row = (maximum row) - (minimum row)

sanitise :: [String] -> [[Int]]
sanitise rows = (fmap . fmap) read $ fmap words rows

partOne :: IO Int
partOne = do
  n <- numbers
  return $ sum $ fmap diff n

partTwo :: IO Int
partTwo = do
  n <- numbers
  return $ sum $ fmap divisible n

divisible :: [Int] -> Int
divisible row =
  head [(div x y) | x <- row, y <- row, x /= y, let z = rem x y, z == 0]
