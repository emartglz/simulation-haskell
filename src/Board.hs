module Board (board, addCorralsBoard, addObstaclesBoard, filterByTypeCell) where

import Random (rand, randomRange, runRandom)
import Utils (replace, _1)

type Position = (Int, Int)

type CellType = String

type Pick = Bool

type Drop = Bool

type BoardCell = (CellType, Pick, Drop)

type PositionBoardCell = (Position, BoardCell)

type Board = [[BoardCell]]

board :: Int -> Int -> Board
board r c
  | r == 0 || c == 0 = []
  | otherwise = boardRow r c : board (r - 1) c

boardRow :: Int -> Int -> [BoardCell]
boardRow r c
  | c == 0 = []
  | otherwise = ("empty", False, False) : boardRow r (c - 1)

boardCellTypeEncounter :: CellType -> Board -> [(Int, Int)]
boardCellTypeEncounter cellType board = boardCellTypeEncounterRow cellType board 0 (length board)

boardCellTypeEncounterRow :: CellType -> Board -> Int -> Int -> [(Int, Int)]
boardCellTypeEncounterRow cellType board r rEnd
  | r == rEnd = []
  | otherwise = boardCellTypeEncounterColmn cellType board r 0 (length (board !! r)) ++ boardCellTypeEncounterRow cellType board (r + 1) rEnd

boardCellTypeEncounterColmn :: CellType -> Board -> Int -> Int -> Int -> [(Int, Int)]
boardCellTypeEncounterColmn cellType board r c cEnd
  | c == cEnd = []
  | _1 (board !! r !! c) == cellType = (r, c) : boardCellTypeEncounterColmn cellType board r (c + 1) cEnd
  | otherwise = boardCellTypeEncounterColmn cellType board r (c + 1) cEnd

addObstaclesBoard :: Int -> Int -> Board -> Board
addObstaclesBoard seed amount board
  | amount == 0 = board
  | otherwise = addObstaclesBoard rR (amount - 1) boardR
  where
    emptyCells = boardCellTypeEncounter "empty" board
    randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
    (rR, rC) = emptyCells !! randomCell
    boardR = replace board rR (replace (board !! rR) rC ("obstacle", False, False))

get4AdyacentCells :: (Int, Int) -> Board -> [PositionBoardCell]
get4AdyacentCells (r, c) board = result
  where
    rows = length board
    columns = length (head board)
    up
      | r == 0 = []
      | otherwise = [((r -1, c), board !! (r - 1) !! c)]
    down
      | r == (rows - 1) = []
      | otherwise = [((r + 1, c), board !! (r + 1) !! c)]
    left
      | c == 0 = []
      | otherwise = [((r, c - 1), board !! r !! (c - 1))]
    right
      | c == (columns - 1) = []
      | otherwise = [((r, c + 1), board !! r !! (c + 1))]
    result = up ++ down ++ left ++ right

filterByTypeCell :: CellType -> [PositionBoardCell] -> [PositionBoardCell]
filterByTypeCell _ [] = []
filterByTypeCell cellType ((p, x) : xs)
  | _1 x == cellType = (p, x) : filterByTypeCell cellType xs
  | otherwise = filterByTypeCell cellType xs

addCorralsBoard :: Int -> Int -> Board -> Board
addCorralsBoard seed amount board
  | amount == 0 = board
  | null corralsAlready =
    let emptyCells = boardCellTypeEncounter "empty" board
        randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
        (rR, rC) = emptyCells !! randomCell
        boardR = replace board rR (replace (board !! rR) rC ("corral", False, False))
     in addCorralsBoard rR (amount - 1) boardR
  | otherwise =
    let corralCells = boardCellTypeEncounter "corral" board
        corralWithAdyacents = map (\(r, c) -> ((r, c), get4AdyacentCells (r, c) board)) corralCells
        corralWithEmptyAdyacents = map (\(p, l) -> (p, filterByTypeCell "empty" l)) corralWithAdyacents
        corralWithEmptyAdyacentsNotNull = filter (\(p, l) -> not (null l)) corralWithEmptyAdyacents

        randomCorral = randomRange 0 (length corralWithEmptyAdyacentsNotNull - 1) (runRandom rand seed)
        ((rR, rC), l) = corralWithEmptyAdyacentsNotNull !! randomCorral

        randomEmpty = randomRange 0 (length l - 1) (runRandom rand rR)
        ((rER, rEC), _) = l !! randomEmpty

        boardR = replace board rER (replace (board !! rER) rEC ("corral", False, False))
     in addCorralsBoard rER (amount - 1) boardR
  where
    corralsAlready = boardCellTypeEncounter "corral" board
