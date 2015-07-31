module AVeryBigSum where

import Control.Monad

read' :: String -> inte

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = foldl (+) (map (read) str)
  mapM_ print ans