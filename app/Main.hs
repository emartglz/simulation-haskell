module Main where

-- import Brick
import Board
import Child
import Robot
import System.Random
import Utils

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

  let childMoveProbability = 1 / 2
  let trashProbability = 1 / 2

  loop (seed + 4) boardWithRobot childMoveProbability trashProbability

loop :: Int -> Board -> Float -> Float -> IO ()
loop seed board childMoveProbability trashProbability = do
  let boardWithChildMoved = moveChilds seed childMoveProbability trashProbability board
  let boardWithRobotMoved = moveRobots boardWithChildMoved
  printBoard boardWithRobotMoved

  putStrLn "press Enter"
  input1 <- getLine
  let r = (read input1 :: Int)

  loop (seed + 1) boardWithRobotMoved childMoveProbability trashProbability