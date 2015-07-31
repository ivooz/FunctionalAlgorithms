import Control.Monad
import Data.List

main = do
    n <- readLn :: IO Int
    solutions <- forM [1..n] (\a -> do
        problem)
    mapM putStrLn $ map show solutions

problem = do
    x <- readLn :: IO Int
    return $ count  (show x) x

count :: [Char] -> Int -> Int
count [] _ = 0
count (x:xs) n
    | x == '0' = count xs n
    | mod n (read $ x : []) == 0 = 1 + count xs n
    | otherwise = count xs n