module Day15 where

import Data.Bits
import Data.List

genA = (512, 16807)
genB = (191, 48271)

partOne = countMatches a b
  where
    n = 40000000
    a = take n $ generate genA
    b = take n $ generate genB

partTwo = countMatches a b
  where
    n = 5000000
    a = take n $ filter (\n -> mod n 4 == 0) $ generate genA
    b = take n $ filter (\n -> mod n 8 == 0) $ generate genB

countMatches a b =
  sum $ zipWith (\a b -> if a == b then 1 else 0) a b

generate (seed, factor) =
    drop 1 $ unfoldr (\b -> Just (toBinary b, rem (b * factor) 2147483647)) seed

toBinary :: Integer -> Integer
toBinary n = (.&. 0xffff) n
