import Control.Monad

read' :: String -> Integer
read' x = read x

main :: IO ()
main = do
    n <- readLn :: IO Int
    rows <- forM [1..n] (\a -> do
        row <- getLine
        return row)
    let
        ans = abs ((diag1 rows 0) - (diag2 rows (length rows -  1)))
    print ans

toIntRow :: String -> [Integer]
toIntRow x = map read' $ words x

diag1 [] _ = 0
diag1 (x:xs) n = ((toIntRow x) !! n) + (diag1 xs (n+1))

diag2 [] _ = 0
diag2 (x:xs) n = ((toIntRow x) !! n) + (diag2 xs (n-1))