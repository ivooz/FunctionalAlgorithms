import Control.Monad

-- 1 2 3 6 7 14 15 30 ...
main = do
    n <- getLine
    inputs <- replicateM (read n) getLine
    mapM print $ map (\x -> foldl grow 1 $ take (read x) [1,2..]) inputs

grow x n
    | n `mod` 2 == 0 = x+1
    | otherwise = 2*x