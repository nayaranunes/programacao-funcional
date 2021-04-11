import Data.Array

main::IO()
main = print(serie 4 12.2)

numeradores:: Integer -> Double -> Array Integer Double
numeradores n x = array (1,n) [(i,x^i)| i <- [1..n]]

fatorial:: Integer -> Integer
fatorial n 
   |n == 0 = 1
   |otherwise = n * fatorial(n -1)

serie::Integer -> Double -> Double
serie n x = 1 + (foldr1 (+) [(((numeradores n x)!a)) / (fromIntegral (fatorial a)) | a <- [1..n]])
