module Robot (moveRobots, generateDistanceTable, generateBoard, expandDistance) where

import Board
import Random
import Utils

type PositionDistanceCell = (Position, Int)

type DistanceBoard = [[PositionDistanceCell]]

maxConstant = 9999999

moveRobots :: Board -> Board
moveRobots board =
  let boardRobot = moveRobotsList (boardCellTypeEncounter "robot" board) board
      boardRobotCorral = moveRobotsList (boardCellTypeEncounter "robot-corral" board) boardRobot
      boardRobotChild = moveRobotsChildList (boardCellTypeEncounter "robot-child" board) boardRobotCorral
      boardRobotChildCorral = moveRobotsChildCorralList (boardCellTypeEncounter "robot-child-corral" board) boardRobotChild
      boardRobotTrash = moveRobotsTrashList (boardCellTypeEncounter "robot-trash" board) boardRobotChildCorral
      boardRobotChildTrash = moveRobotsChildTrashList (boardCellTypeEncounter "robot-child-trash" board) boardRobotTrash
   in boardRobotChildTrash

moveRobotsList :: [PositionBoardCell] -> Board -> Board
moveRobotsList [] board = board
moveRobotsList ((p, c) : xs) board =
  let dboard = generateDistanceTable p ["child", "trash"] ["robot", "robot-corral", "robot-child-corral", "child", "robot-child", "robot-trash", "obstacle", "robot-child-trash"] board
      childs = boardCellTypeEncounter "child" board
      trash = boardCellTypeEncounter "trash" board
      boardR =
        if null childs
          then
            if null trash
              then board --dont move
              else
                let trashPositions = map getPosition trash
                    (trashWithLowerDistance, trashDistance) = calculateLowerDistanceList (head trashPositions) trashPositions dboard maxConstant
                 in if trashDistance == maxConstant
                      then board --cant reach trash
                      else
                        let path = getPathFromDistance trashWithLowerDistance dboard []
                            (rDestiny, cDestiny) = walkNCells 1 path
                            (start, finish) = typeOfCellChange (p, c) (board !! rDestiny !! cDestiny)
                         in replaceInBoardList [start, finish] board
          else
            let childsPositions = map getPosition childs
                (childWithLowerDistance, childDistance) = calculateLowerDistanceList (head childsPositions) childsPositions dboard maxConstant
             in if childDistance == maxConstant
                  then --cant reach child, search for trash

                    let trashPositions = map getPosition trash
                        (trashWithLowerDistance, trashDistance) = calculateLowerDistanceList (head trashPositions) trashPositions dboard maxConstant
                     in if trashDistance == maxConstant
                          then board --cant reach trash
                          else
                            let path = getPathFromDistance trashWithLowerDistance dboard []
                                (rDestiny, cDestiny) = walkNCells 1 path
                                (start, finish) = typeOfCellChange (p, c) (board !! rDestiny !! cDestiny)
                             in replaceInBoardList [start, finish] board
                  else
                    let path = getPathFromDistance childWithLowerDistance dboard []
                        (rDestiny, cDestiny) = walkNCells 1 path
                        (start, finish) = typeOfCellChange (p, c) (board !! rDestiny !! cDestiny)
                     in replaceInBoardList [start, finish] board
   in moveRobotsList xs boardR

moveRobotsChildList :: [PositionBoardCell] -> Board -> Board
moveRobotsChildList [] board = board
moveRobotsChildList ((p, c) : xs) board =
  let dboard = generateDistanceTable p [corralConstant] ["robot", "robot-corral", "robot-child-corral", "child", "robot-child", "robot-trash", "obstacle", "robot-child-trash"] board
      corrals = boardCellTypeEncounter corralConstant board
      boardR =
        if null corrals
          then board --dont move
          else
            let corralsPositions = map getPosition corrals
                (corralWithLowerDistance, corralDistance) = calculateLowerDistanceList (head corralsPositions) corralsPositions dboard maxConstant
             in if corralDistance == maxConstant
                  then board --cant reach corral, just hold child forever FIX THIS
                  else
                    let path = getPathFromDistance corralWithLowerDistance dboard []
                        (rDestiny, cDestiny) = walkNCells 2 path
                        (start, finish) = typeOfCellChange (p, c) (board !! rDestiny !! cDestiny)
                     in replaceInBoardList [start, finish] board
   in moveRobotsChildList xs boardR

moveRobotsChildCorralList :: [PositionBoardCell] -> Board -> Board
moveRobotsChildCorralList _ board = board

moveRobotsTrashList :: [PositionBoardCell] -> Board -> Board
moveRobotsTrashList [] board = board
moveRobotsTrashList ((p, c) : xs) board =
  let boardR = board
   in moveRobotsTrashList xs boardR

moveRobotsChildTrashList :: [PositionBoardCell] -> Board -> Board
moveRobotsChildTrashList [] board = board
moveRobotsChildTrashList ((p, c) : xs) board =
  let boardR = board
   in moveRobotsChildTrashList xs boardR

generateDistanceTable :: Position -> [CellType] -> [CellType] -> Board -> DistanceBoard
generateDistanceTable p destinationList obstacleList board =
  let r = length board
      c = length (head board)
      db = generateBoard r c
      dbWith0 = replaceInBoard (p, 0) db
   in expandDistance [(p, 0)] destinationList obstacleList board dbWith0

expandDistance :: [PositionDistanceCell] -> [CellType] -> [CellType] -> Board -> DistanceBoard -> DistanceBoard
expandDistance [] _ _ _ dboard = dboard
expandDistance (((row, column), distance) : xs) destinationList obstaclesList board dboard =
  let adyacents = get4AdyacentCells (row, column) board

      adyacentsFDestinationCellType = filter (\(_, (c, _, _)) -> isMember c destinationList) adyacents
      adyacentsFDestinationDistance = filter (\((r, c), _) -> distance + 1 < getBoardCell (dboard !! r !! c)) adyacentsFDestinationCellType
      adyacentsDestinationDistance = map (\(p, _) -> (p, distance + 1)) adyacentsFDestinationDistance
      dboardRR = replaceInBoardList adyacentsDestinationDistance dboard

      adyacentsFCellType = filter (\(_, (c, _, _)) -> not (isMember c obstaclesList)) adyacents
      adyacentsFDistance = filter (\((r, c), _) -> distance + 1 < getBoardCell (dboard !! r !! c)) adyacentsFCellType
      adyacentsDistance = map (\(p, _) -> (p, distance + 1)) adyacentsFDistance
      dboardR = replaceInBoardList adyacentsDistance dboardRR
   in expandDistance (xs ++ adyacentsDistance) destinationList obstaclesList board dboardR

generateBoard :: Int -> Int -> DistanceBoard
generateBoard rEnd cEnd
  | rEnd == 0 || cEnd == 0 = []
  | otherwise = boardFormer 0 rEnd cEnd

boardFormer :: Int -> Int -> Int -> DistanceBoard
boardFormer r rEnd cEnd
  | r == rEnd = []
  | otherwise = boardRow r 0 cEnd : boardFormer (r + 1) rEnd cEnd

boardRow :: Int -> Int -> Int -> [PositionDistanceCell]
boardRow r c cEnd
  | c == cEnd = []
  | otherwise = ((r, c), maxConstant) : boardRow r (c + 1) cEnd

calculateLowerDistanceList :: Position -> [Position] -> DistanceBoard -> Int -> PositionDistanceCell
calculateLowerDistanceList p [] _ mini = (p, mini)
calculateLowerDistanceList p (position : xs) dboard mini
  | minInAdyacents < mini = calculateLowerDistanceList position xs dboard minInAdyacents
  | otherwise = calculateLowerDistanceList p xs dboard mini
  where
    adyacents = get4AdyacentCells position dboard
    minInAdyacents = foldr min maxConstant (map (\(_, x) -> x) adyacents)

getPathFromDistance :: Position -> DistanceBoard -> [Position] -> [Position]
getPathFromDistance (r, c) dboard path
  | getBoardCell (dboard !! r !! c) == 0 = path
  | otherwise =
    let pathR = (r, c) : path
        adyacents = get4AdyacentCells (r, c) dboard
        (d, pR) = minimum (map (\(p, x) -> (x, p)) adyacents)
     in getPathFromDistance pR dboard pathR

walkNCells :: Int -> [Position] -> Position
walkNCells steps path
  | steps > length path = path !! (length path - 1)
  | otherwise = path !! (steps - 1)

typeOfCellChange :: PositionBoardCell -> PositionBoardCell -> (PositionBoardCell, PositionBoardCell)
typeOfCellChange start finish
  --robot
  | ini == robotConstant && end == emptyConstant = ((pini, (emptyConstant, False, False)), (pend, (robotConstant, False, False)))
  | ini == robotConstant && end == childConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildConstant, True, False)))
  | ini == robotConstant && end == trashConstant = ((pini, (emptyConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | ini == robotConstant && end == corralConstant = ((pini, (emptyConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  --robot-child
  | ini == robotChildConstant && end == emptyConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildConstant, False, False)))
  | ini == robotChildConstant && end == trashConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildTrashConstant, False, False)))
  | ini == robotChildConstant && end == corralConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildCorralConstant, False, False)))
  --robot-trash
  | ini == robotTrashConstant && end == emptyConstant = ((pini, (trashConstant, False, False)), (pend, (robotConstant, False, False)))
  | ini == robotTrashConstant && end == childConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildConstant, True, False)))
  | ini == robotTrashConstant && end == trashConstant = ((pini, (trashConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | ini == robotTrashConstant && end == corralConstant = ((pini, (trashConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  --robot-corral
  | ini == robotCorralConstant && end == emptyConstant = ((pini, (corralConstant, False, False)), (pend, (robotConstant, False, False)))
  | ini == robotCorralConstant && end == childConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildConstant, True, False)))
  | ini == robotCorralConstant && end == trashConstant = ((pini, (corralConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | ini == robotCorralConstant && end == corralConstant = ((pini, (corralConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  --robot-child-trash
  | ini == robotChildTrashConstant && end == emptyConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildConstant, False, False)))
  | ini == robotChildTrashConstant && end == trashConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildTrashConstant, False, False)))
  | ini == robotChildTrashConstant && end == corralConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildCorralConstant, False, False)))
  --robot-child-corral drop
  | drop && ini == robotChildCorralConstant && end == emptyConstant = ((pini, (childCorralConstant, False, False)), (pend, (robotConstant, False, False)))
  | drop && ini == robotChildCorralConstant && end == trashConstant = ((pini, (childCorralConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | drop && ini == robotChildCorralConstant && end == corralConstant = ((pini, (childCorralConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  --robot-child-corral
  | ini == robotChildCorralConstant && end == emptyConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildConstant, False, False)))
  | ini == robotChildCorralConstant && end == trashConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildTrashConstant, False, False)))
  | ini == robotChildCorralConstant && end == corralConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildCorralConstant, False, False)))
  --
  | otherwise = (start, finish)
  where
    ini = getCellType (getBoardCell start)
    drop = getDrop (getBoardCell start)
    end = getCellType (getBoardCell finish)
    pini = getPosition start
    pend = getPosition finish
