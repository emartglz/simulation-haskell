module Utils (replace, getPosition, getBoardCell, getRow, getColumn, getCellType) where

replace :: [a] -> Int -> a -> [a]
replace list index element = let (first, x : xs) = splitAt index list in first ++ (element : xs)

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