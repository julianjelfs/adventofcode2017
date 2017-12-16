module Main where

import           Day4
import           Day5
import           Day7
import           Day13
import           Day14
import           Day15
import           Day16

main :: IO ()
main = do
  --putStrLn ("Solution " ++ (show Day15.partOne))
  p2 <- Day16.partTwo
  case p2 of
    Left _ -> putStrLn "Error"
    Right s -> putStrLn $ "Solution: " ++ s
