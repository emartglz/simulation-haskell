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

  putStrLn "enter value for obstacles: "
  input3 <- getLine
  let obstacles = (read input3 :: Int)

  putStrLn "enter value for kids: "
  input4 <- getLine
  let kids = (read input4 :: Int)

  putStrLn "enter value for robots: "
  input5 <- getLine
  let robots = (read input5 :: Int)

  let boardEmpty = board r c

  g <- newStdGen
  let seed = fst (random g)
  let boardWithCorrals = addCorralsBoard seed kids boardEmpty
  let boardWithObstacles = addGenericBoard (seed + 1) "obstacle" obstacles boardWithCorrals
  let boardWithChild = addGenericBoard (seed + 2) "child" kids boardWithObstacles
  let boardWithRobot = addGenericBoard (seed + 3) "robot" robots boardWithChild
  print boardWithRobot
