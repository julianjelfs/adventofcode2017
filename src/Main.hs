module Main where

import           Day4
import           Day5
import           Day7
import           Day13

main :: IO ()
main = do
  n <- Day13.partTwo
  putStrLn ("Solution " ++ (show n))
