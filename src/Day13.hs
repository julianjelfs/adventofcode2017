module Day13 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M

partOne = (\s -> trip s 0) <$> parse <$> readFile "data/day13.txt"

partTwo = do
  inp <- readFile "data/day13.txt"
  let scanners = parse inp
      trips = fmap (trip scanners) [0 ..]
  return $ take 1 $ filter (\(n, (c, s)) -> not c) $ trips

parse inp =
  foldr
    foldLine
    M.empty
    ((fmap (read :: String -> Int) . splitOn ": ") <$> lines inp)
  where
    foldLine (d:r:[]) m = M.insert d r m
    foldLine _ _ = error "invalid format"

trip scanners delay = (delay, foldl' (step delay scanners) (False, 0) [0 .. 90])

isCaught delay range = rem delay (2 * (range - 1)) == 0

step delay scanners (caught, sev) depth =
  case M.lookup depth scanners of
    Nothing -> (caught || False, sev)
    Just r ->
      case isCaught (delay + depth) r of
        True -> (True, sev + (depth * r))
        _ -> (caught || False, sev)
