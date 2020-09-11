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

--5--

bissexto::Int->Bool
bissexto x
  |mod x 4 == 0 && mod x 100 /= 0 = True
  |mod x 4 == 0 && mod x 100 == 0 && mod x 400 == 0 = True
  |otherwise = False

bissextos::[Int]->[Int]
bissextos lista = [x | x <- lista, bissexto x]

--6--

sublistas::[[Int]]->[Int]
sublistas lista = [x | sub<-lista, x<-sub]

--7--

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
[("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

bissexto::Integer->Bool
bissexto x
  |mod x 4 == 0 && mod x 100 /= 0 = True
  |mod x 4 == 0 && mod x 100 == 0 && mod x 400 == 0 = True
  |otherwise = False

valida::Data->Bool
valida (d,m,a)
  |d <= 0 || d > 31 = False
  |m <= 0 || m > 12 = False
  |a < 0 = False
  |((m == 4)||(m == 6)||(m == 9)||(m == 11)) && d > 30 = False
  |(bissexto a) && (m == 2) && (d > 29) = False
  |not(bissexto a) && (m == 2) && (d > 28) = False
  |otherwise = True

precede::Data->Data->Bool
precede(d1,m1,a1) (d2,m2,a2)
  |not(valida(d1,m1,a1)) = False
  |not(valida(d2,m2,a2)) = False
  |a1 < a2 = True
  |a1 == a2 && m1 < m2 = True
  |a1 == a2 && m1 == m2 && d1 < d2 = True
  |otherwise = False

emDia::Data->Emprestimo->Bool
emDia dataAtual (livro, aluno, dataRetirada, dataDevolucao, status) =
  precede dataAtual dataDevo

atrasados::Emprestimos->Data->[Emprestimos]
atrasados lista dataAtual = [x | x <- lista, not (emDia dataAtual x)]

--8--

npares::[Int]->Int
npares [] = 0
npares (x:xs)
  |even x = 1 + (npares xs)
  |otherwise = (npares xs)

--9--

produtorio::[Int]->Int
produtorio [x] = x
produtorio (x:xs) = x * (produtorio xs)

--10--

comprime::[[Int]]->[Int]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

--11--

tamanho::Num a=>[a]->a  --polimorfica
tamanho [] = 0
tamanho (x:xs) = 1 + (tamanho xs)

--12--

uniaoNRec::[Int]->[Int]
uniaoNRec lista = 

uniaoRec [] [] = []
uniaoRec (x:xs) (y:ys)
  |x == y = x:uniaoRec xs ys
  |otherwise = uniaoRec xs ys


--12)
membro :: Eq t => t -> [t] -> Bool
membro n [] = False
membro n (x:xs)
  | n == x = True
  | otherwise = membro n xs

uniaoNRec :: [Int]->[Int]->[Int]
uniaoNRec l1 l2 = [x | x <- l1] ++ [y | y <- l2, not (membro y l1)]

--13)
uniaoRec2 :: [Int] -> [Int] -> [Int]
uniaoRec2 (a : as) (x : xs)
  | (a == x) = uniaoRec2 as xs
  | otherwise = a : as ++ x : xs
