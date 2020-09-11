ou1::Bool->Bool->Bool
ou2::Bool->Bool->Bool
ou3::Bool->Bool->Bool
ou4::Bool->Bool->Bool
ou5::Bool->Bool->Bool

--casamento de padrões--

ou1 True True = True
ou1 True False = True
ou1 False True = True
ou1 False False = False

ou2 False False = False
ou2 _ _ = True

ou3 False x = x
ou3 True _ = True

--expresões condicionais--

ou4 x y 
  |x == True = True
  |y == True = True
  |otherwise = False

ou5 x y 
  |x == False && x == y = False
  |otherwise = True

--2--

distancia::(Float, Float)->(Float, Float)->Float
distancia (x1,y1) (x2,y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

--3--

-- 1:[2,3,4] = [1,2,3,4]
-- 'a':['b','c','d'] = "abcd"
-- head [1,2,3] = 1
-- tail [1,2,3] = [2,3]
-- [1,5,2,3]!!1 = 5 primeiro elem tail 
-- [1,5,2,3]!!3 = 3 terceiro elem tail
-- elem 2 [1,5,2,3] = True
-- take 2 [1,5,2,3,7] = [1,5]
-- drop 2 [1,5,2,3,7] = [2,3,7]
-- [1,2] ++ [3,4] = [1,2,3,4]
-- [1..10] = [1,2,3,4,5,6,7,8,9,10]
-- [7,6..3] = [7,6,5,4,3]
-- ['b'..'g'] = "bcdefg"
-- take 5 [1,3..] = [1,3,5,7,9]
-- sum [1..10] = 55
-- maximum [1,5,2,3,7] = 7
-- minimum [1,5,2,3,7] = 1

--4--

fatorial1::Int->Int
fatorial2::Int->Int

fatorial1 0 = 1
fatorial1 n = n * fatorial1(n-1)

fatorial2 n
  |n == 0 = 1
  |otherwise = n * fatorial2(n-1)

--5--

fibo::Int->Int
fibo 0 = 0
fibo 1 = 1
fibo 2 = 1
fibo n = (fibo (n-2))+(fibo (n-1))

--6--

n_tri::Int->Int
n_tri 0 = 0
n_tri 1 = 1
n_tri n = n + n_tri (n-1)

--7--

passo::(Int,Int)->(Int,Int)
passo (x,y) = (y,x+y)

fiboAux::Int->(Int,Int)
fiboAux n 
  |n == 0 = (0,1)
  |otherwise = passo (fiboAux (n-1))

fibo2::Int->Int
fibo2 n = fst(fiboAux n)

--8--

potencia2::Int->Int
potencia2 0 = 1
potencia2 n = 2* (potencia2 (n-1))

--9--

prodIntervalo::Int->Int->Int
prodIntervalo m n
  |m == n = n
  |m<n = m*(prodIntervalo (m+1) (n))

fatorial2::Int->Int
fatorial2 x = prodIntervalo 1 x

--10 nao tem numero 10 na lista --

--11--

resto_div::Int->Int->Int
resto_div m n 
  |n>(m-n) = (m-n)
  |otherwise = resto_div (m-n) n
  
div_inteira::Int->Int->Int
div_inteira m n 
  |m==n = 1
  |otherwise = (resto_div m n)*n

--12--

mdc::(Int,Int)->Int
mdc (m,0) = m
mdc (m,n) = mdc(n,(mod m n))

mdc2::(Int,Int)->Int
mdc2 (m,n) 
  |n == 0 = m
  |otherwise = mdc2(n,(mod m n))

--13--

binomial::(Int,Int)->Int
binomial (n,k)
  |k == 0 = 1
  |k == n = 1
  |k < n = ((binomial (n-1,k))+(binomial (n-1,k-1)))

binomial2::(Int,Int)->Int
binomial2 (n,0) = 1
binomial2 (n,k) = if(k == n) then 1
		else ((binomial2 (n-1,k))+(binomial2 (n-1,k-1)))

--14--

[5,4..1] = [5,4,3,2,1]
['a','c'..'e'] = "ace"
[1,4..16] = [1,4,7,10,13,16]
zip [1, -2..(-11)] [1,5..17] = [(1,1),(-2,5),(-5,9),(-8,13),(-11,17)]

--15--

list :: Int -> Int -> [Int]
list x y
    | x == y = [x]
    | x > y = []
    | otherwise = x:(list (x+1) y)

listPar :: Int -> Int -> [Int]
listPar x y
    | x == y = []
    | x+1 == y = []
    | x > y = []
    | mod x 2 == 0 = listPar (x+1) y
    | otherwise = (x+1):(listPar (x+1) y)
