import Control.Monad

main :: IO ()
main = do
    n <- getLine
    let
        ans = convert $ splitAt 8 n
    putStrLn ans

convert (a,b)
    | (fst $ splitAt 2 a) == "12" = handle12 (a,b)
    | b == "AM" = a
    | otherwise = convertToPM $ splitAt 2 a

handle12 (a,b)
    | b == "PM" = a
    | otherwise = "00" ++ (snd $ splitAt 2 a)

convertToPm ("12":x,y) = x
convertToPM (x,y) = (show . (+12) . read) x ++ y