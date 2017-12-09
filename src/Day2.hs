{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import           Common
import           Data.Char
import qualified Text.Parsec as P

numbers :: IO [[Int]]
numbers = do
  inp <- readFile "data/day2.txt"
  return $ parse inp

parse :: String -> [[Int]]
parse inp =
  case P.parse fileParser "day2.txt" inp of
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

rowParser = P.sepBy numberParser P.tab

fileParser = P.sepBy rowParser P.newline

testLine = "j inc 19"

wordParser = P.many P.anyChar

instructionParser =
  Instr <$> (wordParser <* space) <*> (opParser <* space) <*> (numberParser)

space = P.char ' '

opParser = chooseOp <$> (P.choice [P.string "inc", P.string "dec"])
  where
    chooseOp "inc" = Inc
    chooseOp "dec" = Dec

--compParser =
--  chooseOp <$> (P.choice [P.string ">=", P.string "<="])
--  where chooseOp ">=" = GE
--        chooseOp "<=" = LE
data Instr =
  Instr String
        Op
        Int
  deriving (Show)

data Op
  = Inc
  | Dec
  deriving (Show)

data Comp
  = GE
  | LE
  deriving (Show)
