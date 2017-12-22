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

main :: IO ()
main = do
  n <- Day22.partTwo
  putStrLn ("Solution: " ++ (show n))
