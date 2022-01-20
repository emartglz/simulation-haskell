module Utils (replace, getPosition, getBoardCell, getRow, getColumn, getCellType, replaceInBoard, swapPosition, printBoard) where

replace :: [a] -> Int -> a -> [a]
replace list index element = let (first, x : xs) = splitAt index list in first ++ (element : xs)

replaceInBoard :: ((Int, Int), (String, Bool, Bool)) -> [[((Int, Int), (String, Bool, Bool))]] -> [[((Int, Int), (String, Bool, Bool))]]
replaceInBoard ((r, c), x) board = replace board r (replace (board !! r) c ((r, c), x))

swapPosition :: (Int, Int) -> (Int, Int) -> [[((Int, Int), (String, Bool, Bool))]] -> [[((Int, Int), (String, Bool, Bool))]]
swapPosition (originR, originC) (destinyR, destinyC) board =
  replaceInBoard ((originR, originC), y) (replaceInBoard ((destinyR, destinyC), x) board)
  where
    x = getBoardCell (board !! originR !! originC)
    y = getBoardCell (board !! destinyR !! destinyC)

getPosition :: (p, c) -> p
getPosition (p, c) = p

getBoardCell :: (p, c) -> c
getBoardCell (p, c) = c

getCellType :: (String, Bool, Bool) -> String
getCellType (c, p, d) = c

getRow :: (Int, Int) -> Int
getRow (r, c) = r

getColumn :: (Int, Int) -> Int
getColumn (r, c) = c

printBoard :: [[((Int, Int), (String, Bool, Bool))]] -> IO ()
printBoard [] = print ""
printBoard (x : xs) = do
  print x
  printBoard xs