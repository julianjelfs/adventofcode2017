module Day12 where

import           Data.List.Split
import qualified Data.Set as S
import qualified Data.Map.Strict as M

partOne = do
  m <- parse
  return $ length $ findGroup m S.empty "0"

partTwo = do
  m <- parse
  return $ countGroups m (S.fromList $ M.keys m) 0

countGroups m remaining groups
    | length remaining == 0 = groups
    | otherwise =
        let v = findGroup m S.empty (head $ S.toList remaining)
            d = S.difference remaining v
        in
            countGroups m d (groups + 1)

findGroup m visited k =
  let connected = m M.! k
      v = S.insert k visited
      unvisited = S.difference connected v
  in
    case length unvisited of
      0 -> v
      _ -> foldl (\v u -> findGroup m v u) v unvisited

parse = do
  inp <- readFile "data/day12.txt"
  return $ foldl (\m l -> parseLine m $ words l) M.empty (lines inp)

parseLine m (p:"<->":ps:[]) = M.insert p (S.fromList $ splitOn "," ps) m
parseLine _ _ = error "unexpected format"
