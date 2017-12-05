module Day5 where

import qualified Data.Sequence as S

data State = State Int (S.Seq Int) deriving Show

parseData :: IO State
parseData = do
  inp <- readFile "data/day5.txt"
  return $ State 0 (S.fromList (fmap read $ lines inp))

jump :: State -> Maybe State
jump (State i list) =
  fmap movenext (itemOf i list)
    where
      movenext instr = State (i + instr) (S.update i (instr + 1) list)

itemOf :: Int -> S.Seq a -> Maybe a
itemOf x xs
  | x < 0 = Nothing
  | x >= length xs = Nothing
  | otherwise = Just (S.index xs x)


solve :: (Int, Maybe State) -> Int
solve (n, Nothing) = n - 1
solve (n, Just (State i l)) = solve (n + 1, jump (State i l))

partOne :: IO Int
partOne = do
  state <- parseData
  return $ solve (0, Just state)

test =
  solve (0, Just (State 0 (S.fromList [0,3,0,1,-3])))

s1 = (State 0 (S.fromList [0,3,0,1,-3]))
s2 = (State 0 (S.fromList [1,3,0,1,-3]))
s3 = (State 1 (S.fromList [2,3,0,1,-3]))
s4 = (State 4 (S.fromList [2,4,0,1,-3]))
s5 = (State 1 (S.fromList [2,4,0,1,-2]))
s6 = (State 5 (S.fromList [2,5,0,1,-2]))

