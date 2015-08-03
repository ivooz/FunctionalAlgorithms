import Control.Monad

main = do
    n <- readLn :: IO Int
    rows <- forM [1..n] (\a -> do
        getLine)
    mapM putStrLn $ groupCH (solve rows) n

solve :: [String] -> [Char]
solve xs = [isCavity xs i j | let last = (length xs) -1,
    i <- [0..last],
    j <- [0..last]]

isCavity :: [String] -> Int -> Int -> Char
isCavity xs i j
    | i==0 || j==0 || i==max || j==max = elem
    | otherwise = isCavity' xs i j
    where max = (length xs) -1
          elem = xs !! i !! j

isCavity' xs i j
    | all (<elem) [up,down,left,right] = 'X'
    | otherwise = elem
    where up = xs !! (i-1) !! j
          down = xs !! (i+1) !! j
          right = xs !! i !! (j+1)
          left = xs !! i !! (j-1)
          elem = xs !! i !! j

groupCH :: [Char] -> Int -> [String]
groupCH [] _ = []
groupCH xs n = (toStr $ take n xs ) : (groupCH (drop n xs) n)

toStr [] = []
toStr (x:xs) = x : (toStr xs)