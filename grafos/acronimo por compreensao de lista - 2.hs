import Data.Char
main :: IO ()
main = print("Grafos - ex2")

acronimo::String->String
acronimo frase = findAcronimo frase

findAcronimo::String->String
findAcronimo frase = [ x | x <- frase, isUpper x]
