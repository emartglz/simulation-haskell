module Board (board, addObstaclesBoard) where

import Random (rand, randomRange, runRandom)
import Utils (replace, _1)

type CellType = String

type Pick = Bool

type Drop = Bool

type BoardCell = (CellType, Pick, Drop)

type Board = [[BoardCell]]

board :: Int -> Int -> Board
board r c
  | r == 0 || c == 0 = []
  | otherwise = boardRow r c : board (r - 1) c

boardRow :: Int -> Int -> [BoardCell]
boardRow r c
  | c == 0 = []
  | otherwise = ("empty", False, False) : boardRow r (c - 1)

boardEmptySpaces :: Board -> [(Int, Int)]
boardEmptySpaces board = boardEmptySpacesRow board 0 (length board)

boardEmptySpacesRow :: Board -> Int -> Int -> [(Int, Int)]
boardEmptySpacesRow board r rEnd
  | r == rEnd = []
  | otherwise = boardEmptySpacesColumn board r 0 (length (board !! r)) ++ boardEmptySpacesRow board (r + 1) rEnd

boardEmptySpacesColumn :: Board -> Int -> Int -> Int -> [(Int, Int)]
boardEmptySpacesColumn board r c cEnd
  | c == cEnd = []
  | otherwise = case _1 (board !! r !! c) :: String of
    "empty" -> (r, c) : boardEmptySpacesColumn board r (c + 1) cEnd
    _ -> boardEmptySpacesColumn board r (c + 1) cEnd

addObstaclesBoard :: Int -> Int -> Board -> Board
addObstaclesBoard seed amount board
  | amount == 0 = board
  | otherwise = addObstaclesBoard rR (amount - 1) boardR
  where
    emptyCells = boardEmptySpaces board
    randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
    (rR, rC) = emptyCells !! randomCell
    boardR = replace board rR (replace (board !! rR) rC ("obstacle", False, False))