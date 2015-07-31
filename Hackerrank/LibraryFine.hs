import Control.Monad

read' :: String -> Integer
read' x = read x

toIntRow :: String -> [Integer]
toIntRow x = map read' $ words x

main = do
    xs <- getLine
    ys <- getLine
    print $ calculate (toIntRow xs) (toIntRow ys)

calculate xs ys
    | (reverse xs) <= (reverse ys) = 0
    | yearR /= yearB = 10000
    | monthR /= monthB = (monthR - monthB) * 500
    | otherwise = (dayR-dayB) * 15
    where
        yearB = ys !! 2
        yearR = xs !! 2
        monthB = ys !! 1
        monthR = xs !! 1
        dayB = ys !! 0
        dayR = xs !! 0

