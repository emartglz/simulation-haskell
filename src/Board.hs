module Board (board, addCorralsBoard, addObstaclesBoard, filterByTypeCell, addChildBoard) where

import Random
import Utils

type CellType = String

type Pick = Bool

type Drop = Bool

type Position = (Int, Int)

type BoardCell = (CellType, Pick, Drop)

type PositionBoardCell = (Position, BoardCell)

type Board = [[PositionBoardCell]]

board :: Int -> Int -> Board
board rEnd cEnd
  | rEnd == 0 || cEnd == 0 = []
  | otherwise = boardFormer 0 rEnd cEnd

boardFormer :: Int -> Int -> Int -> Board
boardFormer r rEnd cEnd
  | r == rEnd = []
  | otherwise = boardRow r 0 cEnd : boardFormer (r + 1) rEnd cEnd

boardRow :: Int -> Int -> Int -> [PositionBoardCell]
boardRow r c cEnd
  | c == cEnd = []
  | otherwise = ((r, c), ("empty", False, False)) : boardRow r (c + 1) cEnd

boardCellTypeEncounter :: CellType -> Board -> [PositionBoardCell]
boardCellTypeEncounter cellType board = boardCellTypeEncounterRow cellType board 0 (length board)

boardCellTypeEncounterRow :: CellType -> Board -> Int -> Int -> [PositionBoardCell]
boardCellTypeEncounterRow cellType board r rEnd
  | r == rEnd = []
  | otherwise = boardCellTypeEncounterColmn cellType board r 0 (length (board !! r)) ++ boardCellTypeEncounterRow cellType board (r + 1) rEnd

boardCellTypeEncounterColmn :: CellType -> Board -> Int -> Int -> Int -> [PositionBoardCell]
boardCellTypeEncounterColmn cellType board r c cEnd
  | c == cEnd = []
  | getCellType (getBoardCell (board !! r !! c)) == cellType = (board !! r !! c) : boardCellTypeEncounterColmn cellType board r (c + 1) cEnd
  | otherwise = boardCellTypeEncounterColmn cellType board r (c + 1) cEnd

addObstaclesBoard :: Int -> Int -> Board -> Board
addObstaclesBoard seed amount board
  | amount == 0 = board
  | otherwise = addObstaclesBoard rR (amount - 1) boardR
  where
    emptyCells = boardCellTypeEncounter "empty" board
    randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
    ((rR, rC), _) = emptyCells !! randomCell
    boardR = replace board rR (replace (board !! rR) rC ((rR, rC), ("obstacle", False, False)))

get4AdyacentCells :: Position -> Board -> [PositionBoardCell]
get4AdyacentCells (r, c) board = result
  where
    rows = length board
    columns = length (head board)
    up
      | r == 0 = []
      | otherwise = [board !! (r - 1) !! c]
    down
      | r == (rows - 1) = []
      | otherwise = [board !! (r + 1) !! c]
    left
      | c == 0 = []
      | otherwise = [board !! r !! (c - 1)]
    right
      | c == (columns - 1) = []
      | otherwise = [board !! r !! (c + 1)]
    result = up ++ down ++ left ++ right

filterByTypeCell :: CellType -> [PositionBoardCell] -> [PositionBoardCell]
filterByTypeCell _ [] = []
filterByTypeCell cellType ((p, x) : xs)
  | getCellType x == cellType = (p, x) : filterByTypeCell cellType xs
  | otherwise = filterByTypeCell cellType xs

addCorralsBoard :: Int -> Int -> Board -> Board
addCorralsBoard seed amount board
  | amount == 0 = board
  | null corralsAlready =
    let emptyCells = boardCellTypeEncounter "empty" board
        randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
        ((rR, rC), _) = emptyCells !! randomCell
        boardR = replace board rR (replace (board !! rR) rC ((rR, rC), ("corral", False, False)))
     in addCorralsBoard rR (amount - 1) boardR
  | otherwise =
    let corralCells = boardCellTypeEncounter "corral" board
        corralWithAdyacents = map (\((r, c), x) -> (((r, c), x), get4AdyacentCells (r, c) board)) corralCells
        corralWithEmptyAdyacents = map (\(x, l) -> (x, filterByTypeCell "empty" l)) corralWithAdyacents
        corralWithEmptyAdyacentsNotNull = filter (\(_, l) -> not (null l)) corralWithEmptyAdyacents

        randomCorral = randomRange 0 (length corralWithEmptyAdyacentsNotNull - 1) (runRandom rand seed)
        (((rR, rC), _), l) = corralWithEmptyAdyacentsNotNull !! randomCorral

        randomEmpty = randomRange 0 (length l - 1) (runRandom rand rR)
        ((rER, rEC), _) = l !! randomEmpty

        boardR = replace board rER (replace (board !! rER) rEC ((rER, rEC), ("corral", False, False)))
     in addCorralsBoard rER (amount - 1) boardR
  where
    corralsAlready = boardCellTypeEncounter "corral" board

addChildBoard :: Int -> Int -> Board -> Board
addChildBoard seed amount board
  | amount == 0 = board
  | otherwise =
    let emptyCells = boardCellTypeEncounter "empty" board
        randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
        ((rR, rC), _) = emptyCells !! randomCell
        boardR = replace board rR (replace (board !! rR) rC ((rR, rC), ("child", False, False)))
     in addChildBoard rR (amount - 1) boardR
