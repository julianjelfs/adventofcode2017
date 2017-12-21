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

main :: IO ()
main = do
    res <- Day21.solve 18
    case res of
        Right n -> putStrLn ("Solution " ++ (show n))
        Left _ -> putStrLn "something went wrong"
