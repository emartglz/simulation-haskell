module Main where

-- import Brick
import Board
import System.Random

-- ui :: Widget ()
-- ui = str "Hello, world!"

-- main :: IO ()
-- main = simpleMain ui

main :: IO ()
main = do
  putStrLn "enter value for rows: "
  input1 <- getLine
  putStrLn "enter value for columns: "
  input2 <- getLine
  let r = (read input1 :: Int)
  let c = (read input2 :: Int)
  let b = board r c
  g <- newStdGen
  let seed = fst (random g)
  let bWithObstacles = addObstaclesBoard seed 1 b
  print bWithObstacles
