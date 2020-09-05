--1--

lst1=[x*2 | x <- [1..10], x*2 >= 12]
=> [12,14,16,18,20]  
lst2=[ x | x <- [50..100], mod x 7 == 3]
=> [52,59,66,73,80,87,94]   
lst3=[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
=> [10,11,12,14,16,17,18,20]   
lst4=[(x,y)| x <- [1..4], y <- [x..5]]
=> [(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]

--2--

quadrados::Int->Int->[Int]
quadrados a b = [x*x | x <- [a..b]]

--3--

selecionaImpares::[Int]->[Int]
selecionaImpares lista = [x | x <- lista, not (even x)]

--4--

tabuada::Int->[Int]
tabuada x = [x*y | y <- [1..10]]
