import Control.Monad

read' :: String -> Int
read' x = read x

toIntRow :: String -> [Int]
toIntRow x = map read' $ words x

main = do
    n <- readLn :: IO Int
    rows <- forM [1..n] (\a -> do
        problem)
    mapM putStrLn rows

problem :: IO String
problem = do
    x <- getLine
    let k = (toIntRow x) !! 1
    students <- getLine
    return $ verify (length . filter (<=0) $ toIntRow students) k

verify :: Int -> Int -> String
verify x k
    | x<k = "YES"
    | otherwise = "NO"