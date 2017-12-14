module Main where

import           Day4
import           Day5
import           Day7
import           Day13
import           Day14

main :: IO ()
main = do
  two <- Day14.partTwo
  putStrLn ("Solution " ++ (show two))
