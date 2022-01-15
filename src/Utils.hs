module Utils (replace, _1) where

replace :: [a] -> Int -> a -> [a]
replace list index element = let (first, x : xs) = splitAt index list in first ++ (element : xs)

_1 :: (a, b, c) -> a
_1 (a, b, c) = a