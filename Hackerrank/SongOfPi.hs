import Control.Monad
import Data.List

pi' = "31415926535897932384626433833"

main = do
    n <- readLn :: IO Int
    solutions <- forM [1..n] (\a -> do
        problem)
    mapM putStrLn solutions

problem = do
    x <- getLine
    return $ findPi (words x) pi'

findPi :: [String] -> String -> String
findPi [] _ =  "It's a pi song."
findPi (x:xs) (p:ps)
    | (show $ length x) == (p : []) = findPi xs ps
    | otherwise = "It's not a pi song."
