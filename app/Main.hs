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
  let r = (read input1 :: Int)

  putStrLn "enter value for columns: "
  input2 <- getLine
  let c = (read input2 :: Int)

  putStrLn "enter value for kids: "
  input3 <- getLine
  let k = (read input3 :: Int)

  let boardEmpty = board r c

  g <- newStdGen
  let seed = fst (random g)
  let boardWithCorrals = addCorralsBoard seed k boardEmpty
  let boardWithObstacles = addGenericBoard (seed + 1) "obstacle" 1 boardWithCorrals
  let boardWithChild = addGenericBoard (seed + 1) "child" k boardWithObstacles
  print boardWithChild
