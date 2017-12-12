module Day12 where

import           Data.List.Split
import qualified Data.Set as S
import qualified Data.Map.Strict as M

solve = do
  inp <- readFile "data/day12test.txt"
  let m = foldl (\m l -> parse m $ words l) M.empty (lines inp)
      v = partOne m S.empty "0"
  return $ length v

partOne m visited k =
  let connected = m M.! k
      v = S.insert k visited
      unvisited = S.difference connected v
  in
    case length unvisited of
      0 -> v
      _ -> foldl (\v u -> partOne m v u) v unvisited

parse m (p:"<->":ps:[]) = M.insert p (S.fromList $ splitOn ", " ps) m
parse _ _ = error "unexpected format"
