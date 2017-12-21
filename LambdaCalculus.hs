module LambdaCalculus where
import Data.Function
import Control.Exception.Base

data LambdaCalculusException
  = NegativeNumber
  | ForbiddenNumber
  | EmptyList
  deriving (Show, Eq)

instance Exception LambdaCalculusException

square :: (Num a) => a -> a
square = \x -> x * x

pow :: Int -> Int -> Int
pow = fix (\f x y ->
  if y == 0 then 1
  else if y < 0 then throw (NegativeNumber)
  else x * f x (y-1))

fatorial :: Int -> Int
fatorial = fix (\f x ->
  if x < 0 then throw (NegativeNumber)
  else if x == 0 then 1
  else x * f (x-1))

isPrime :: Int -> Bool
isPrime = \x -> if x <= 0 then False
  else if x == 1 then True
  else length (filter (\y -> mod x y == 0) [2.. floor(sqrt (fromIntegral x))] ) == 0

fib :: Int -> Integer
fib = fix (\f x -> if x < 0 then throw (NegativeNumber)
  else if x == 0 then 0
  else if x == 1 then 1
  else f (x-1) + f (x-2))

mdc :: Int -> Int -> Int
mdc = fix (\f x y -> if y == 0 then x else f (abs y) ((abs x) `mod` (abs y)) )

mmc :: Int -> Int -> Int
mmc = \x y -> if x == 0 && y == 0 then 0
  else (x * y) `div` (mdc x y)

coprimo :: Int -> Int -> Bool
coprimo = \x y -> if (mdc x y) == 1 then True else False

goldbach :: Int -> [(Int, Int)]
goldbach = \x -> do
  let primos = filter isPrime [1..(x-1)]
  if x > 2 then [ (a, b) | a <- primos, b <- primos, a + b == x]
  else throw (ForbiddenNumber)

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast :: [a] -> a
meuLast = fix (\f xs ->
  if null xs then throw (EmptyList)
  else if null (tail xs) then (head xs)
  else f (tail xs) )

penultimo :: [a] -> a
penultimo = fix (\f xs ->
  if null xs then throw (EmptyList)
  else if null (tail xs) then throw (EmptyList)
  else last (init xs) )

elementAt :: Int -> [a] -> a
elementAt = fix (\f i xs ->
  if (null xs) then throw (EmptyList)
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
compact = fix (\f xs -> if null xs then []
  else if ((head xs) `elem` (tail xs)) then [head xs] ++ (filter (== (head xs)) (tail xs)) ++ compact (filter (/= (head xs)) (tail xs))
  else [head xs] ++ f (tail xs) )

encode :: (Eq a) => [a] -> [(a, Int)]
encode = fix ( \f xs -> if null xs then []
  else ( (head xs), length (filter (== (head xs)) (tail xs)) + 1 ) : f (filter (/= (head xs)) (tail xs) )
  )

split :: Int -> [a] -> [[a]]
split = \i xs -> [take i xs] ++ [drop i xs]

slice :: [a] -> Int -> Int -> [a]
slice = fix (\f xs imin imax -> if (imin == 1) && (imax == 0) then []
  else if (imin == 1) then (head xs) : f (tail xs) 1 (imax - 1)
  else f (tail xs) (imin - 1) (imax - 1)  )

insertAt :: a -> Int -> [a] -> [a]
insertAt = fix (\f el pos xs -> if (pos == 1) && (null xs) then [el]
  else if (pos == 1) then [el] ++ xs
  else (head xs) : (f el (pos-1) (tail xs) ) )

remove :: (Eq a) => a -> [a] -> [a]
remove = fix (\f e xs -> if e == (head xs) then (tail xs)
  else (head xs):(f e (tail xs)) )

sort :: (Ord a) => [a] -> [a]
sort = fix (\f xs -> if null xs then []
  else (minimum xs) : (f (remove (minimum xs) xs) ) )

mySum :: (Num a) => [a] -> a
mySum = \xs -> foldr (+) 0 xs

maxList :: (Ord a, Num a) => [a] -> a
maxList = \xs -> if null xs then throw (EmptyList)
  else foldr max 0 xs

buildPalindrome :: [a] -> [a]
buildPalindrome = fix (\f xs -> if null xs then []
  else [head xs] ++ f (tail xs) ++ [head xs] )

mean :: (Integral a, Foldable t, Floating b) => t a -> b
mean = \xs -> if null xs then throw (EmptyList)
  else fromIntegral(sum xs) / fromIntegral(length xs)

myAppend :: [a] -> [a] -> [a]
myAppend = \xs ys -> foldr (:) ys xs
