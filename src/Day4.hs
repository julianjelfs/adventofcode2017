module Day4 where

import qualified Data.Set as S
import Data.List

phrases :: (String -> [[String]]) -> IO [[String]]
phrases parser = do
  inp <- readFile "data/day4.txt"
  return $ parser inp

noDuplicates :: [String] -> Bool
noDuplicates p =
  length p == (length (S.fromList p))

countValid :: (String -> [[String]]) -> IO Int
countValid parser = do
  p <- phrases parser
  return $ length $ filter noDuplicates p

partOne :: IO Int
partOne =
  countValid $ (fmap words) . lines

partTwo :: IO Int
partTwo =
  countValid $ ((fmap . fmap) sort) . (fmap words) . lines
