import Data.Function
import Test.QuickCheck

--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

pow :: Int -> Int -> Int
pow = fix (\f x y ->
  if y == 0 then 1
  else if y < 0 then error "Negative exponent"
  else x * f x (y-1))

fatorial :: Int -> Int
fatorial = fix (\f x ->
  if x < 0 then error "Negative input"
  else if x == 0 then 1
  else x * f (x-1))

isPrime :: Int -> Bool
isPrime = \x -> if x <= 0 then False
  else if x == 1 then True
  else length (filter (\y -> mod x y == 0) [2.. floor(sqrt (fromIntegral x))] ) == 0

fib :: Int -> Integer
fib = fix (\f x -> if x == 1 then 1
  else if x == 2 then 1
  else f (x-1) + f (x-2))

mdc :: Int -> Int -> Int
mdc = fix (\f x y -> if y == 0 then x else f y (x `mod` y))

mmc :: Int -> Int -> Int
mmc = \x y -> (x * y) `div` (mdc x y)

coprimo :: Int -> Int -> Bool
coprimo = \x y -> if (mdc x y) == 1 then True else False

goldbach = \x -> do
  let primos = filter isPrime [1..(x-1)]
  if x > 2 then [ (a, b) | a <- primos, b <- primos, a + b == x]
  else undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast :: (Show a) => [a] -> a
meuLast = fix (\f xs ->
  if null xs then error "Empty list"
  else if null (tail xs) then (head xs)
  else f (tail xs) )

penultimo :: (Show a) => [a] -> a
penultimo = fix (\f xs ->
  if null xs then error "Empty list"
  else last (init xs) )

elementAt :: (Show a) => Int -> [a] -> a
elementAt = fix (\f i xs ->
  if (null xs) then error "Can't reach index"
  else if i == 1 then head xs
  else f (i-1) (tail xs) )

meuLength :: [a] -> Int
meuLength = fix (\f xs ->
  if null xs then 0
  else f (tail xs) + 1 )

meuReverso :: [a] -> [a]
meuReverso = fix (\f xs -> if null xs then []
  else f (tail xs) ++ [head xs] )

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = \x -> if (x == (reverse x)) then True else False

compress :: (Eq a) => [a] -> [a]
compress = fix (\f xs -> if null xs then []
  else f (init xs) ++ [x | x <- [last xs], not( x `elem` (init xs) ) ] )

compact :: (Eq a) => [a] -> [a]
compact = fix(\f xs -> if null xs then []
  else if ((head xs) `elem` (tail xs)) then [head xs] ++ (filter (== (head xs)) (tail xs)) ++ compact (filter (/= (head xs)) (tail xs))
  else [head xs] ++ f (tail xs) )

encode :: (Eq a) => [a] -> [(a, Int)]
encode xs = undefined

split :: Int -> [a] -> [[a]]
split = \i xs -> [take i xs] ++ [drop i xs]

slice xs imin imax = undefined

insertAt el pos xs = undefined

sort xs = undefined

mySum :: (Num a) => [a] -> a
mySum = \xs -> foldr (+) 0 xs

maxList :: (Ord a, Num a) => [a] -> a
maxList = \xs -> if null xs then error "Empty list"
  else foldr max 0 xs

buildPalindrome :: [a] -> [a]
buildPalindrome = fix (\f xs -> if null xs then []
  else [head xs] ++ f (tail xs) ++ [head xs] )

mean :: (Integral a, Foldable t, Fractional b) => t a -> b
mean = \xs -> fromIntegral(sum xs) / fromIntegral(length xs)

myAppend :: [a] -> [a] -> [a]
myAppend = \xs ys -> foldr (:) ys xs

{- TESTES QC -}

prop_pow :: Int -> Int -> Property
prop_pow x y = not (y <= 0) ==> exponenciation x y
  where
  exponenciation x 0 = 1 == pow x 0
  exponenciation 0 y = 0 == pow 0 y
  exponenciation x y = product(replicate y x) == pow x y

prop_fatorial :: Int -> Property
prop_fatorial x = not (x <= 0) ==> fatorial' x
  where
  fatorial' x = (product [1..x]) == fatorial x

prop_isPrime_model :: Int -> Bool
prop_isPrime_model x = myIsPrime x == isPrime x
  where
  myIsPrime x = if x <= 0 then False
    else if x == 1 then True
    else length (filter (\y -> mod x y == 0) [2.. floor(sqrt (fromIntegral x))] ) == 0
