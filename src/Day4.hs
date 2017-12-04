module Day4 where

import qualified Data.Set as S
import Data.List

type Policy = [String] -> Bool

phrases :: IO [[String]]
phrases = do
  inp <- readFile "data/day4.txt"
  return $ parse inp

parse :: String -> [[String]]
parse = (fmap words) . lines

isAnagram :: String -> String -> Bool
isAnagram a b = sort a == sort b

noDuplicates :: Policy
noDuplicates p =
  length p == (length (S.fromList p))

noAnagrams :: Policy
noAnagrams p =
  length (anagrams p) == 0

anagrams :: [String] -> [(String, String)]
anagrams p =
    [(x, y) | x <- p, y <- p, x /= y, isAnagram x y]

countValid :: Policy -> IO Int
countValid policy = do
  p <- phrases
  return
    (sum $
      fmap
        (\p ->
          if policy p then
            1
          else
            0
        ) p)

partOne :: IO Int
partOne =
  countValid noDuplicates

partTwo :: IO Int
partTwo =
  countValid noAnagrams

testRow = words "xbmaf iceyqqq sabpt gliexel muubepe qqiyqce fmrcc eazk obkeonl fmccr kgk"
