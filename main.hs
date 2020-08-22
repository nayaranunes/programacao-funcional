main::IO()
main = print("Funcoes disponiveis: dobro, quadruplo, hipotenusa,distancia, converte, bissexto, bissexto2, valida, precede, emDia")

dobro::Float->Float
dobro x = x + x

quadruplo::Float->Float
quadruplo x = dobro(dobro x)

hipotenusa::Float->Float->Float
hipotenusa a b = sqrt((a * a) + (b * b))

distancia::(Float, Float)->(Float, Float)->Float
distancia (x1,y1) (x2,y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

converte::Float->(Float,Float,Float)
converte x = (x, x * 3.96 ,x * 4.45)

bissexto::Integer->Bool
bissexto x
  |mod x 4 == 0 && mod x 100 /= 0 = True
  |mod x 4 == 0 && mod x 100 == 0 && mod x 400 == 0 = True
  |otherwise = False

type Data = (Integer, Integer, Integer)
bissexto2::Data->Bool
bissexto2 (d,m,a)
  |bissexto a == True = True
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

type Livro = (String,String,String,String,Integer)
type Aluno = (String,String,String,String)
type Emprestimo = (String,String,Data,Data,String)

e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")
e2 = ("H214C5","BCC198674",(14,7,2019),(15,3,2019),"aberto")

emDia::Emprestimo->Bool
emDia (_,_,(d1,m1,a1),(d2,m2,a2),_)
  |precede (d1,m1,a1) (d2,m2,a2) = True
  |otherwise = False
