import Control.Monad

read' :: String -> Int
read' x = read x

toIntRow :: String -> [Int]
toIntRow x = map read' $ words x

main = do
    n <- readLn :: IO Int
    solutions <- forM [1..n] (\a -> do
        problem)
    mapM print solutions

problem = do
    nums <- getLine
    return $ alg 1 (toIntRow nums !! 0) (toIntRow nums !! 1)

alg x z y
    | product > y = 0
    | product <= y && product>= z = 1 + alg (x+1) z y
    | otherwise = alg (x+1) z y
    where
        product = x*x