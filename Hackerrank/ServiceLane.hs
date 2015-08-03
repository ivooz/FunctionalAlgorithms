import Control.Monad

read' :: String -> Int
read' x = read x

toIntRow :: String -> [Int]
toIntRow x = map read' $ words x

main = do
    n <- getLine
    lane <- getLine
    rows <- forM [1..((toIntRow n) !! 1)] (\a -> do
        problem $ toIntRow lane)
    mapM print rows

problem :: [Int] -> IO Int
problem lane = do
    x <- getLine
    let k = toIntRow x
    return $ foldr min 5 $ drop (k !! 0) $ take (k !! 1 + 1) lane

