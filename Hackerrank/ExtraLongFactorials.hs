import Control.Monad

main = do
  n <- readLn :: IO Integer
  let result = fac n
  print result

fac 0 = 1
fac n = n * fac (n-1)