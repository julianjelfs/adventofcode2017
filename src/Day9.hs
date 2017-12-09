module Day9 where

solve = do
  inp <- readFile "data/day9.txt"
  return $ foldl foldData (0, 0, False, False, 0) inp

foldData (level, score, False, _, total) '{' = (level + 1, score + level + 1, False, False, total)
foldData (level, score, False, _, total) '}' = (level - 1, score, False, False, total)
foldData (level, score, False, _, total) '<' = (level, score, True, False, total)
foldData acc@(_, _, False, _, _) _ = acc
foldData (level, score, True, True, total) _ = (level, score, True, False, total)
foldData (level, score, True, False, total) '!' = (level, score, True, True, total)
foldData (level, score, True, False, total) '>' = (level, score, False, False, total)
foldData (level, score, True, False, total) _ = (level, score, True, False, total + 1)
