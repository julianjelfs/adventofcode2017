module Main where

import           Day4
import           Day5
import           Day7

main :: IO ()
main = do
  n <- Day7.partTwo
  putStrLn ("Solution " ++ (show n))
