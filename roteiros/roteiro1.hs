main :: IO ()
main = print("Available functions: double, quadruple, hypotenuse, distance")

double::Float->Float
double x = x * 2

quadruple::Float->Float
quadruple x = double(double x)

hypotenuse::Float->Float->Float
hypotenuse a b = sqrt((a * a) + (b * b))

distance::(Float, Float)->(Float, Float)->Float
distance (x1,y1) (x2,y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

e1 = 1 + 2 * 3 => 7
e2 = 5 ^ 3 => 125
e3 = 5 ** 3 => 125.0
e4 = 5 / 3 => 1.6666666666666667
e5 = div 5 3 => 1
e6 = mod 5 3 => 2
e7 = 5 < 3 => False
e8 = mod 5 3 < 2 => False
e9 = mod 5 3 == 2 => True
e10 = sqrt 81 => 9.0
e11 = logBase 2 1024 => 10 .0
e12 = floor 5.7 => 5
e13 = ceiling 5.7 => 6 
e14 = abs (-5) => 5
e15 = min 6 7 => 6
e16 = max 6 7 => 7
e17 = sin (pi/2) => 1.0
e18 = sum [1..5] => 15
e19 = not True => False
e20 = True && False => False 

