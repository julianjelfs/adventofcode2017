module Day15 where

import Data.Bits
import Data.List
import Text.Printf

genA = (512, 16807)
genB = (191, 48271)
--genA = (65, 16807)
--genB = (8921, 48271)

partOne = countMatches a b
  where
    a = generate genA 40000000
    b = generate genB 40000000

countMatches a b =
  length $ filter id $ zipWith (==) a b

generate g n = fmap (toBinary . fst) $ drop 1 $ scanl' gen g [0 .. (n - 1)]
  where
    gen (prev, factor) _ = (rem (prev * factor) 2147483647, factor)

toBinary :: Integer -> Integer
toBinary n = (.&. 0xffff) n
