module Day9 where

solve = do
  inp <- readFile "data/day9.txt"
  return $ foldl foldData (0, 0, False, False, 0) inp
  where foldData acc@(level, score, garbage, skip, total) c =
          case (garbage, skip, c) of
            (False, _, '{') -> (level + 1, score + level + 1, False, False, total)
            (False, _, '}') -> (level - 1, score, False, False, total)
            (False, _, '<') -> (level, score, True, False, total)
            (False, _, _) -> acc
            (True, True, _) ->  (level, score, True, False, total)
            (True, False, '!') -> (level, score, True, True, total)
            (True, False, '>') -> (level, score, False, False, total)
            (True, False, _) -> (level, score, True, False, total + 1)
