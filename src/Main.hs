module Main where

import Day5
import Day4
import Day7

main :: IO ()
main = do
    n <- Day7.partTwo
    putStrLn ("Solution " ++ (show n))
