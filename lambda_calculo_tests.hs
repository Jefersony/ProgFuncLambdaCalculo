import Data.Function

--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

pow = fix (\f x y -> if y == 0 then 1 else x * f x (y-1))

fatorial = fix (\f x -> if x == 0 then 1 else x * f (x-1))

isPrime = \x -> if x == 1 then True
  else length (filter (\y -> mod x y ==0) [2.. floor(sqrt (fromIntegral x))] ) == 0

fib = fix (\f x -> if x == 1 then 1 else if x == 2 then 1 else f (x-1) + f (x-2))

mdc = fix (\f x y -> if y == 0 then x else f y (x `mod` y))

mmc = \x y -> (x * y) `div` (mdc x y)

coprimo = \x y -> if (mdc x y) == 1 then True else False

goldbach = \x -> do
  let primos = filter isPrime [1..(x-1)]
  if x > 2 then [ (a, b) | a <- primos, b <- primos, a + b == x]
  else undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast xs = undefined
penultimo xs = undefined
elementAt i xs = undefined
meuLength xs = undefined
meuReverso xs = undefined
isPalindrome xs = undefined
compress xs = undefined
compact xs = undefined
encode xs = undefined
split xs i = undefined
slice xs imin imax = undefined
insertAt el pos xs = undefined
sort xs = undefined
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean xs = undefined
myAppend xs ys = undefined
