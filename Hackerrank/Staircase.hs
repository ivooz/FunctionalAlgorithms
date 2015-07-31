import Control.Monad

main = do
    n <- readLn :: IO Int
    mapM putStrLn $ reverse $ take n $ staircase n n

staircase 0 _= [[]]
staircase x n = (step x n) : (staircase (x-1) n)

step 0 _ = []
step x n = ( foldr (:) [] $ take (n-x) [' ',' '..] ) ++ ( foldr (:) [] $ take (x) ['#','#'..] )