module Child (moveChilds) where

import Board
import Random
import Utils

moveChilds :: Int -> Float -> Board -> Board
moveChilds seed moveProbability board = moveChildsList seed moveProbability (boardCellTypeEncounter "child" board) board

moveChildsList :: Int -> Float -> [PositionBoardCell] -> Board -> Board
moveChildsList _ _ [] board = board
moveChildsList seed moveProbability ((p, cell) : xs) board =
  let randomChoice = randomRange 0 100 (runRandom rand seed)
      boardR
        | randomChoice <= round (moveProbability * 100) =
          let moveChoices = get4AdyacentCells p board
              randomMoveIndex = randomRange 0 (length moveChoices) (runRandom rand randomChoice)
              cellDestiny = moveChoices !! randomMoveIndex
              boardRR
                | getCellType (getBoardCell cellDestiny) == "empty" || getCellType (getBoardCell cellDestiny) == "obstacle" =
                  let direction = (getRow (getPosition cellDestiny) - getRow p, getColumn (getPosition cellDestiny) - getColumn p)
                      destinySwap = getFirstCellLine p p direction "empty" "obstacle" board
                   in swapPosition p (getPosition cellDestiny) (swapPosition (getPosition cellDestiny) (getPosition destinySwap) board)
                | otherwise = board
           in boardRR
        | otherwise = board
   in moveChildsList seed moveProbability xs boardR
