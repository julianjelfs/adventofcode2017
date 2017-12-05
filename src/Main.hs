module Main where

import Day5
import Day4

main :: IO ()
main = do
    n <- Day5.partTwo
    putStrLn ("Solution " ++ (show n))
