-- 1 --
-- a --

bissexto::Integer->Bool
bissexto ano = if1 || if2
  where
	if1 = (mod ano 4 == 0 && mod ano 100 /= 0)
  	if2 = (mod ano 4 == 0 && mod ano 100 == 0 && mod ano 400 == 0)

type Data = (Integer,Integer,Integer)
valida::Data->Bool
valida (d,m,a) = if1 || if2 || if3 || if4 
  where
	if1 = not ((d <= 0 || d > 31) || (m <= 0 || m > 12) || (a < 0))
  	if2 = not (((m == 4)||(m == 6)||(m == 9)||(m == 11)) && (d > 30))
  	if3 = not ((bissexto a) && (m == 2) && (d > 29))
  	if4 = not ((not(bissexto a)) && (m == 2) && (d > 28))

-- b --
bissextos::[Integer]->[Integer]
bissextos lista = if1
  where
    if1 = [x | x <- lista, bissexto x]

-- c --

type Emprestimo = (String,String,Data,Data,String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procede::Data->Data->Bool
procede (dia, mes, ano) (dia2, mes2, ano2) = not (if1 || if2 || if3 || if4)
  where
    if1 = not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2))
    if2 = ano > ano2
    if3 = ano == ano2 && mes > mes2
    if4 = ano == ano2 && mes == mes && dia > dia2

emprestimoEmDia::Data->Emprestimo->Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) = if1
  where
    if1 = procede dataAtual dataDevo

atrasados::Emprestimos->Data->Emprestimos
atrasados listaEmprestimos dataAtual = if1
  where
    if1 = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

-- d --

passo::(Integer,Integer)->(Integer,Integer)
passo (x, y) = if1
  where
    if1 = (y, x + y)

fibo2::Integer->(Integer,Integer)
fibo2 0 = (0, 1)
fibo2 n = if1
  where
    if1 = passo (fibo2 (n -1))

-- e --

prodIntervalo::Integer->Integer->Integer
prodIntervalo m n = if (m >= n) then if1 else if2
  where
    if1 = n
    if2 = (m * (prodIntervalo (m + 1) n))

fatInter::Integer->Integer
fatInter n = if1
  where
    if1 = prodIntervalo 1 n

-- 2 --
-- a --

bissextoLet::Int->Bool
bissextoLet ano =
  let if1 = (mod ano 400 == 0)
      if2 = (mod ano 4 == 0)
      if3 = (mod ano 100 /= 0)
   in if1 || (if2 && if3)

type DataLet = (Int, Int, Int)

validaLet::DataLet->Bool
validaLet (dia, mes, ano) =
  let if1 = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
      if2 = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
      if3 = dia >= 1 && dia <= 28 && mes == 2 && not (bissextoLet ano)
      if4 = dia >= 1 && dia <= 29 && mes == 2 && (bissextoLet ano)
   in if1 || if2 || if3 || if4

-- b --
bissextosLet::[Integer]->[Integer]
bissextosLet lista =
  let if1 = [x | x <- lista, bissextoLet x]
   in if1

-- c --

type EmprestimoLet = (String,String,DataLet,DataLet,String)
type EmprestimosLet = [EmprestimoLet]

bdEmprestimoLet::EmprestimosLet
bdEmprestimoLet =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procedeLet::DataLet->DataLet->Bool
procedeLet (dia, mes, ano) (dia2, mes2, ano2) =
  let if1 = not (validaLet (dia, mes, ano)) || not (validaLet (dia2, mes2, ano2))
      if2 = ano > ano2
      if3 = ano == ano2 && mes > mes2
      if4 = ano == ano2 && mes == mes && dia > dia2
   in not (if1 || if2 || if3 || if4)

emprestimoEmDiaLet::DataLet->EmprestimoLet->Bool
emprestimoEmDiaLet dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) =
  let if1 = procedeLet dataAtual dataDevo
   in if1

atrasadosLet::EmprestimosLet->DataLet->EmprestimosLet
atrasadosLet listaEmprestimos dataAtual =
  let if1 = [x | x <- listaEmprestimos, not (emprestimoEmDiaLet dataAtual x)]
   in if1

-- d --

passoLet::(Integer,Integer)->(Integer,Integer)
passoLet (x, y) =
  let if1 = (y, x + y)
   in if1

fibo2Let::Integer->(Integer,Integer)
fibo2Let 0 = (0, 1)
fibo2Let n =
  let if1 = passoLet (fibo2Let (n -1))
   in if1

-- e --

prodIntegerervaloLet::Integer->Integer->Integer
prodIntegerervaloLet m n =
  let if1 =
        if (m >= n)
          then n
          else (m * (prodIntegerervalo (m + 1) n))
   in if1

fatIntegererLet::Integer->Integer
fatIntegererLet n =
  let if1 = prodIntegerervalo 1 n
   in if1

-- 3 --

-- 3.1 --
--(\x. 2 * x + 1) 3
--2*3 + 1
--6 + 1
--7

-- 3.2 --
--(\ xy. x-y) 5 7
--5 - 7
--(-2)

-- 3.3 --
--(\ yx. x-y) 5 7
--7 - 5
--2

-- 3.4 --
--(\ xy. x-y) (\z. z/2)
--(\y.  (z/2) - y)

-- 3.5 --
--(\ xy. x-y) ((\z. z/2) 6 ) 1
--(\ xy. x-y) (6/2) 1
--(\ xy. x-y) 3 1
--3 - 1
--2

-- 3.6 --
--(\ x. \ y. - x y) 9 4
--(\ x. - x 4) 9 
--( - 9 4) 
--5

-- 3.7 --
--(\ x. xx) (\ y. y)
--(\ y. yy)

-- 4 --

ex1 = (\x -> x + 3) 5 -- 8
ex2 = (\x -> \y -> x * y + 5) 3 4 -- 17
ex3 = (\(x,y) -> x * y^2) (3,4) -- 48
ex4 = (\(x,y,_) -> x * y^2) (3,4,2) -- 48
ex5 = (\xs -> zip xs [1,2,3]) [4,5,6] -- [(4,1),(5,2),(6,3)]

-- 5 --

ex1::Integer
ex1 = (\x -> \y -> y) ((\z -> z) (\z -> z)) (\w -> w) 5 -- 5

ex2::Integer
ex2 = ((\f -> (\x -> f (f x))) (\y -> (y * y))) 3 -- 81

ex3::Integer
ex3 = ((\f -> (\x -> f (f x))) (\y -> (y + y))) 5 -- 20

ex4::Integer
ex4 = ((\x -> (\y -> x + y) 5) ((\y -> y -3) 7)) -- 9

ex5::Integer
ex5 = (((\f -> (\x -> f (f (f x)))) (\y -> (y * y))) 2) -- 256

ex6::Integer
ex6 = (\x -> \y -> x + ((\x -> x - 3) y)) 5 6 -- 8
