module Main where

import           Day4
import           Day5
import           Day7
import           Day13
import           Day14
import           Day15
import           Day16
import           Day17
import           Day21
import           Day22
import           Day23
import           Day24

main :: IO ()
main = do
  a <- Day24.partOne
  b <- Day24.partTwo
  putStrLn ("Part One: " ++ (show a))
  putStrLn ("Part Two: " ++ (show b))
