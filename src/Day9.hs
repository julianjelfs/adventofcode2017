module Day9 where

solve = do
  inp <- readFile "data/day9.txt"
  return $ foldl foldData (0, 0, False, False, 0) inp

foldData acc@(level, score, ingarbage, skip, total) ch =
  case ingarbage of
    False ->
      case ch of
        '{' -> (level + 1, score + level + 1, ingarbage, False, total)
        '}' -> (level - 1, score, False, False, total)
        '<' -> (level, score, True, False, total)
        _ -> acc
    True ->
      case skip of
        True -> (level, score, True, False, total)
        False ->
          case ch of
            '!' -> (level, score, True, True, total)
            '>' -> (level, score, False, False, total)
            _ -> (level, score, ingarbage, skip, total + 1)
