import Control.Monad
import Data.Char

main :: IO ()
main = do
    x <- readLn :: IO Int
    text <- getLine
    n <- readLn :: IO Int
    putStrLn $ alg text n

alg [] n = []
alg (x:xs) n = (shift n x) : (alg xs n)

shift 0 c = c
shift n c = shift (n-1) $ returnNext c

returnNext c
    | c < 'A' = c
    | c > 'Z' && c < 'a' = c
    | c > 'z' = c
returnNext 'Z' = 'A'
returnNext 'z' = 'a'
returnNext c = chr (ord c + 1)
