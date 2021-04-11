main :: IO ()
main = print("Grafos - ex4")

eratosthenes :: [Int] -> [Int]
eratosthenes [] = []
eratosthenes (h:t) = h : (eratosthenes (filter (\x -> mod x h /= 0) t))

fatores :: Int -> [Int]
fatores x = [h | h <- [2..x], mod x h == 0]

fatoresPrimos :: Int -> [Int]
fatoresPrimos x = eratosthenes (fatores x)
