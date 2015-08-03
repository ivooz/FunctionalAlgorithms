-- Naive solution
-- TODO: Karp-Rabin 2D

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
    r <- getLine
    grid <- forM [1..(toIntRow r) !! 0] (\a -> do
            getLine)
    pr <- getLine
    pattern <- forM [1..(toIntRow pr) !! 0] (\a -> do
            getLine)
    return $ resolve $ alg grid pattern

resolve x
    | x = "YES"
    | otherwise = "NO"

alg grid pattern = any id [search grid pattern i j |
    let maxRow = (length grid -1) - ((length pattern) - 1),
    let maxCol = (length (grid !! 0) -1) - ((length (pattern !! 0 )) -1),
    i <- [0..maxRow],
    j <- [0..maxCol]]

search g p i j = all id [ (g !! x !! y) == (p !! (x - i) !! (y - j)) |
    let pRows = length p,
    let pCols = length (p !! 0),
    x <- [i..i+(pRows-1)],
    y <- [j..j+(pCols-1)]]