import Control.Monad

read' :: String -> Int
read' x = read x

toIntRow :: String -> [Int]
toIntRow x = map read' $ words x

main = do
    n <- readLn :: IO Int
    row <- getLine
    mapM print $ simulate $ toIntRow row

simulate :: [Int] -> [Int]
simulate [] = []
simulate xs = (length xs) : (simulate $ rdce xs (foldr min 1001 xs))

rdce [] _ = []
rdce (x:xs) n
    | x == n = rdce xs n
    | otherwise = (x - n) : rdce xs n
