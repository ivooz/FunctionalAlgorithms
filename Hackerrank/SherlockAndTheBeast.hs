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
    | mod x 3 == 0 = x
    | otherwise = alg (x - 5)

resolve x n
    | x == -1 = "-1"
    | otherwise = foldr (++) ("") ((replicate x "5") ++ (replicate (n-x) "3"))