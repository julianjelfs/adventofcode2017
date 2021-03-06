{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import qualified Common as C
import           Data.Char
import qualified Text.Parsec as P

numbers :: IO [[Int]]
numbers = do
  inp <- readFile "data/day2.txt"
  return $ parse inp

parse :: String -> [[Int]]
parse inp =
  case C.parse fileParser inp of
    Right v  -> v
    Left err -> error "Parse error"

diff :: [Int] -> Int
diff row = (maximum row) - (minimum row)

divisible :: [Int] -> Int
divisible row = head [(div x y) | x <- row, y <- row, x /= y, rem x y == 0]

partOne :: IO Int
partOne = solve diff

partTwo :: IO Int
partTwo = solve divisible

solve :: ([Int] -> Int) -> IO Int
solve mapWith = do
  n <- numbers
  return $ sum $ fmap mapWith n

rowParser = P.sepBy C.numberParser P.tab

fileParser = P.sepBy rowParser P.newline
