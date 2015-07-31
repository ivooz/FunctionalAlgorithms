import Control.Monad

main = do
    n <- readLn :: IO Int
    solutions <- forM [1..n] (\a -> do
        problem)
    mapM putStrLn solutions

problem :: IO String
problem = do
    n <- readLn :: IO Int
    let fives = alg n
    return $ resolve fives n

alg :: Int -> Int
alg x
    | x == 0 = 0
    | x<0 = -1
    | div x 5 == 0 = x / 5
    | otherwise = alg (x - 1)

resolve x n
    | x == -1 = "-1"
    | otherwise = foldr (++) ("") ((replicate x "5") ++ (replicate (n-x) "3"))