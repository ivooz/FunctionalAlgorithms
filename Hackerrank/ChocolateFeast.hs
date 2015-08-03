import Control.Monad

read' :: String -> Int
read' x = read x

toIntRow :: String -> [Int]
toIntRow x = map read' $ words x

main = do
    n <- readLn :: IO Int
    rows <- forM [1..n] (\a -> do
        problem)
    mapM print rows

problem = do
    x <- getLine
    let ints = toIntRow x
    let n = ints !! 0
    let c = ints !! 1
    let m = ints !! 2
    return $ (div n c) + extra (div n c) m

extra x n
    | x >= n = newOnes + extra (newOnes+oldOnes) n
    | otherwise = 0
    where newOnes = div x n
          oldOnes = mod x n