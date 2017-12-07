module Day7 where

type Name = String
type Weight = Int
type Children = [Node]

data Node = Node Name Weight Children deriving Show

parse :: IO [Node]
parse = do
  inp <- readFile "data/day7.txt"
  return $ fmap (lineToNode . words) $ lines inp

lineToNode :: [String] -> Node
lineToNode (name : weight : _ : children) =
  Node name (read weight :: Int) (fmap (\c -> Node c 0 []) children)
lineToNode (name : weight) =
  Node name (read (head weight) :: Int) []

