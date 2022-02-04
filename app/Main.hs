module Main where

import Board
import Child
import Robot
import System.Random
import Utils

main :: IO ()
main = do
  putStrLn "enter value for t: "
  input0 <- getLine
  let t = (read input0 :: Int)

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

  putStrLn "enter value for trash: "
  input5 <- getLine
  let trash = (read input5 :: Int)

  putStrLn "enter value for robots: "
  input6 <- getLine
  let robots = (read input6 :: Int)

  putStrLn "enter value for robots ia [1 prefer child, 2 prefer trash]: "
  input7 <- getLine
  let iaRobots = (read input7 :: Int)

  let boardEmpty = board r c

  g <- newStdGen
  let seed = fst (random g)
  let boardWithCorrals = addCorralsBoard seed kids boardEmpty
  let boardWithObstacles = addGenericBoard (seed + 1) "obstacle" obstacles boardWithCorrals
  let boardWithChild = addGenericBoard (seed + 2) "child" kids boardWithObstacles
  let boardWithRobot = addGenericBoard (seed + 3) "robot" robots boardWithChild
  let boardWithTrash = addGenericBoard (seed + 4) "trash" trash boardWithRobot

  let childMoveProbability = 1 / 2
  let trashProbability = 1 / 2
  let turn = 1
  let max_loop = 1000

  printBoard boardWithTrash

  loop (seed + 5) boardWithTrash childMoveProbability trashProbability turn t iaRobots max_loop

loop :: Int -> Board -> Float -> Float -> Int -> Int -> Int -> Int -> IO ()
loop seed board childMoveProbability trashProbability turn t iaRobots max_loop = do
  let boardWithChildMoved = if mod turn t == 0 then moveChilds seed childMoveProbability trashProbability board else board
  let boardWithRobotMoved = moveRobots boardWithChildMoved iaRobots

  putStr ("Turn: " ++ show turn ++ "\n")
  printBoard boardWithRobotMoved

  let trashAmount =
        length (boardCellTypeEncounter trashConstant boardWithRobotMoved)
          + length (boardCellTypeEncounter robotTrashConstant boardWithRobotMoved)
          + length (boardCellTypeEncounter robotChildTrashConstant boardWithRobotMoved)

  let trasheblesCells =
        trashAmount
          + length (boardCellTypeEncounter robotConstant boardWithRobotMoved)
          + length (boardCellTypeEncounter robotChildConstant boardWithRobotMoved)
          + length (boardCellTypeEncounter childConstant boardWithRobotMoved)
          + length (boardCellTypeEncounter emptyConstant boardWithRobotMoved)

  let cleanCells = trasheblesCells - trashAmount

  let corrals = length (boardCellTypeEncounter corralConstant boardWithRobotMoved)

  if cleanCells * 100 < trasheblesCells * 60 && turn < max_loop
    then loop (seed + 1) boardWithRobotMoved childMoveProbability trashProbability (turn + 1) t iaRobots max_loop
    else
      if turn == max_loop
        then putStrLn "max_loop reached"
        else putStr "room clean"