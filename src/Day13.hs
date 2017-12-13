module Day13 where

import           Data.List.Split

partOne = do
  inp <- readFile "data/day13.txt"
  let scanners = (parseScanner . fmap (read::String->Int) . splitOn ": ") <$> lines inp
  return $ foldl step (0, 0, scanners) [0..90]

partTwo n = do
  inp <- readFile "data/day13test.txt"
  --inp <- readFile "data/day13.txt"
  let scanners = (parseScanner . fmap (read::String->Int) . splitOn ": ") <$> lines inp
      delayed = (delayBy scanners) <$> [0..]
      (_, byTen) = delayBy scanners n

  --return $ head $ filter (\(n, (_, s, _)) -> s == 0) $ fmap (\(n, s) -> (n, foldl step (0, 0, s) [0..6])) delayed
  return $ foldl step (0, 0, byTen) [0..6]

parseScanner (d:r:[]) = (d, r, 0, 1)
parseScanner _ = error "invalid format"

delayBy scanners n =
  (n, foldl (\s i -> moveScanner <$> s) scanners [1..n])

step (myDepth, severity, scanners) pico =
  ( myDepth + 1
  , severity + (accumulateSeverity myDepth scanners)
  , moveScanner <$> scanners )

accumulateSeverity myDepth scanners =
  case filter (\(d, r, p, _) -> d == myDepth && p == 0) scanners of
    ((d, r, _, _):[]) -> d * r
    _ -> 0

moveScanner (depth, range, pos, dir)
  | pos == range - 1 && dir == 1 = (depth, range, pos - 1, -1)
  | pos == 0 && dir == -1 = (depth, range, pos + 1, 1)
  | otherwise = (depth, range, pos + dir, dir)
