module Day13 where

import           Data.List.Split
import           Data.List
import qualified Data.Map.Strict as M

partTwo = do
  inp <- readFile "data/day13.txt"
  let scanners = (parseScanner . fmap (read::String->Int) . splitOn ": ") <$> lines inp
      mapped = foldr parseScanner2 M.empty scanners
      delayed = (delayBy mapped) <$> [0..]
  return $ take 1 $ filter (\(n, c) -> not c) $ trip <$> delayed

parseScanner2 (d, r, p, dir) m = M.insert d (r, p, dir) m

parseScanner (d:r:[]) = (d, r, 0, 1)
parseScanner _ = error "invalid format"

delayBy scanners n =
  (n, scanners)

trip (delay, scanners) =
    (delay, foldl' (step delay scanners) False [0..90])

isCaught delay (range, _, _) =
    rem delay (2 * (range - 1)) == 0

step delay scanners caught depth =
    if caught then
        caught
    else
        case isCaught (delay + depth) <$> M.lookup depth scanners of
            Just True -> True
            _ -> caught || False

--partOne = do
--  inp <- readFile "data/day13.txt"
--  let scanners = (parseScanner . fmap (read::String->Int) . splitOn ": ") <$> lines inp
--      mapped = foldr parseScanner2 M.empty scanners
--  return $ trip (0, mapped)

--accumulateSeverity myDepth scanners =
--  case M.lookup myDepth scanners of
--    Just (r, 0, _) -> ((myDepth * r), True)
--    _ -> (0, False)

--moveScanner (range, pos, dir)
--  | pos == range - 1 && dir == 1 = (range, pos - 1, -1)
--  | pos == 0 && dir == -1 = (range, pos + 1, 1)
--  | otherwise = (range, pos + dir, dir)
