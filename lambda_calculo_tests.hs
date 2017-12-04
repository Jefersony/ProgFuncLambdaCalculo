--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
--pow x y = undefined
pow = \x y -> x^y

--fatorial x = undefined
fatorial = \x -> if x == 0 then 1 else x * fatorial (x-1)

--isPrime x = undefined
isPrime = \x -> if x == 1 then True 
            else length (filter (\y -> mod x y == 0) [1..x]) == 2
fib = \x -> if x<=2 then 1
            else fib (x-1) + fib (x-2)

mdc = \x y-> last (mdc2 x y)
mdc2 = \x y -> if mod x y <= 0 then []
                else [mod x y] ++ mdc2 y (mod x y)

mmc = \x y -> if x==0 || y==0 then 0
                else if x == y then x
                  else div (x*y) (mdc x y)

coprimo x y = undefined
goldbach x = undefined

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