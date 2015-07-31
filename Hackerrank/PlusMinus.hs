import Control.Monad
import Text.Printf

read' :: String -> Integer
read' x = read x

first (x, _, _) = x
second (_, x, _) = x
third (_, _, x) = x

fromIntegral' :: Integer -> Float
fromIntegral' x = fromIntegral x :: Float

main = do
    n <- readLn :: IO Integer
    rows <- getLine
    let ans = count (0,0,0) (toIntRow rows)
    mapM (printf "%.3f\n") $ map (\x -> (divide (x ans) n)) [first,second,third]

toIntRow :: String -> [Integer]
toIntRow x = map read' $ words x

count (a,b,c) [] = (a,b,c)
count (a,b,c) (x:xs)
    | x==0 = count (a,b,c+1) xs
    | x<0 = count (a,b+1,c) xs
    | otherwise = count (a+1,b,c) xs

divide :: Integer -> Integer  -> Float
divide x y = ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

