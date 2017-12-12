import Lambda_calculus
import Data.Function
import Test.QuickCheck

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
