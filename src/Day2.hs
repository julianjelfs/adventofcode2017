module Day2 where

import Data.Char

main :: IO ()
main = putStrLn("hello")

numbers :: IO [[Int]]
numbers = do
  inp <- readFile "data/day2.txt"
  return $ sanitise $ lines inp

diff :: [Int] -> Int
diff row =
  (maximum row) - (minimum row)

sanitise :: [String] -> [[Int]]
sanitise rows =
  (fmap . fmap) read $ fmap words rows

solve :: IO Int
solve = do
  n <- numbers
  return $ sum $ fmap diff n
