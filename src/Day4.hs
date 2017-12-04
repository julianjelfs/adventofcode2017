module Day4 where

import qualified Data.Set as S
import Data.List

noDuplicates :: [String] -> Bool
noDuplicates p =
  length p == (length (S.fromList p))

countValid :: (String -> [[String]]) -> IO Int
countValid parser = do
  inp <- readFile "data/day4.txt"
  return $ length $ filter noDuplicates $ parser inp

partOne :: IO Int
partOne =
  countValid $ (fmap words) . lines

partTwo :: IO Int
partTwo =
  countValid $ ((fmap . fmap) sort) . (fmap words) . lines
