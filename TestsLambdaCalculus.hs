module TestsLambdaCalculus where
import LambdaCalculus
import Data.Function
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text
import Data.Text.Lazy.IO as I
import Test.HUnit
import Control.Exception.Base
import Control.Monad as CM

-- Casos de teste para validar as questões de Lambda Calculus
-- Ao executar o main, os erros que acontecerem são exibidos e um arquivo json é criado com o relatório

testPow01 = TestCase (assertEqual "testPow01: 0 elevado a 0" 1 (pow 0 0) )
testPow02 = TestCase (assertEqual "testPow02: 1 elevado a 0" 1 (pow 1 0) )
testPow03 = TestCase (assertEqual "testPow03: 0 elevado a 1" 0 (pow 0 1) )
testPow04 = TestCase (assertEqual "testPow04: 0 elevado a 2" 0 (pow 0 2) )
testPow05 = TestCase (assertEqual "testPow05: 5 elevado a 1" 5 (pow 5 1) )
testPow06 = TestCase (assertEqual "testPow06: 3 elevado a 2" 9 (pow 3 2) )
testPow07 = TestCase (assertException NegativeNumber (evaluate $  pow 2 (-1) ) ) -- Teste para caso excepcional de expoente negativo

testsPow = TestList [ testPow01, testPow02, testPow03, testPow04, testPow05,
  testPow06, testPow07]

testFatorial01 = TestCase (assertEqual "testFatorial01: fatorial de 0" 1 (fatorial 0) )
testFatorial02 = TestCase (assertEqual "testFatorial02: fatorial de 1" 1 (fatorial 1) )
testFatorial03 = TestCase (assertEqual "testFatorial03: fatorial de 2" 2 (fatorial 2) )
testFatorial04 = TestCase (assertEqual "testFatorial04: fatorial de 3" 6 (fatorial 3) )
testFatorial05 = TestCase (assertEqual "testFatorial05: fatorial de 4" 24 (fatorial 4) )
testFatorial06 = TestCase (assertEqual "testFatorial06: fatorial de 10" 3628800 (fatorial 10) )
testFatorial07 = TestCase (assertException NegativeNumber (evaluate $  fatorial (-1) ) ) -- Teste para caso excepcional de expoente negativo

testsFatorial = TestList [testFatorial01, testFatorial02, testFatorial03,
  testFatorial04, testFatorial05, testFatorial06, testFatorial07 ]

testIsPrime01 = TestCase (assertEqual "testIsPrime01: 0 é primo?" False (isPrime 0) )
testIsPrime02 = TestCase (assertEqual "testIsPrime02: -1 é primo?" False (isPrime (-1)) )
testIsPrime03 = TestCase (assertEqual "testIsPrime03: 1 é primo?" True (isPrime 1) )
testIsPrime04 = TestCase (assertEqual "testIsPrime04: 2 é primo?" True (isPrime 2) )
testIsPrime05 = TestCase (assertEqual "testIsPrime05: 3 é primo?" True (isPrime 3) )
testIsPrime06 = TestCase (assertEqual "testIsPrime06: 4 é primo?" False (isPrime 4) )
testIsPrime07 = TestCase (assertEqual "testIsPrime07: 5 é primo?" True (isPrime 5) )
testIsPrime08 = TestCase (assertEqual "testIsPrime08: 222 é primo?" False (isPrime 222) )
testIsPrime09 = TestCase (assertEqual "testIsPrime09: 223 é primo?" True (isPrime 223) )

testsIsPrime = TestList [testIsPrime01, testIsPrime02, testIsPrime03,
  testIsPrime04, testIsPrime05, testIsPrime06, testIsPrime07, testIsPrime08,
  testIsPrime09]

testFib01 = TestCase (assertEqual "testFib01: fibonacci de 0" 0 (fib 0) )
testFib02 = TestCase (assertEqual "testFib02: fibonacci de 1" 1 (fib 1) )
testFib03 = TestCase (assertEqual "testFib03: fibonacci de 2" 1 (fib 2) )
testFib04 = TestCase (assertEqual "testFib04: fibonacci de 3" 2 (fib 3) )
testFib05 = TestCase (assertEqual "testFib05: fibonacci de 4" 3 (fib 4) )
testFib06 = TestCase (assertEqual "testFib06: fibonacci de 5" 5 (fib 5) )
testFib07 = TestCase (assertEqual "testFib07: fibonacci de 6" 8 (fib 6) )
testFib08 = TestCase (assertEqual "testFib08: fibonacci de 15" 610 (fib 15) )
testFib09 = TestCase (assertException NegativeNumber (evaluate $  fib (-1) ) ) -- Teste para caso excepcional (fora do domínio do problema) para fibonacci negativo

testsFib = TestList [testFib01, testFib02, testFib03, testFib04, testFib05,
  testFib06,testFib07,testFib08, testFib09 ]

testMdc01 = TestCase (assertEqual "testeMdc01: mdc entre 0 e 0" 0 (mdc 0 0) )
testMdc02 = TestCase (assertEqual "testeMdc02: mdc entre 0 e 1" 1 (mdc 0 1) )
testMdc03 = TestCase (assertEqual "testeMdc03: mdc entre 1 e 1" 1 (mdc 1 1) )
testMdc04 = TestCase (assertEqual "testeMdc04: mdc entre 2 e 1" 1 (mdc 2 1) )
testMdc05 = TestCase (assertEqual "testeMdc05: mdc entre 2 e 2" 2 (mdc 2 2) )
testMdc06 = TestCase (assertEqual "testeMdc06: mdc entre 5 e 15" 5 (mdc 5 15) )
testMdc07 = TestCase (assertEqual "testeMdc07: mdc entre 7 e 13" 1 (mdc 7 13) )
testMdc08 = TestCase (assertEqual "testeMdc08: mdc entre 225 e 25" 25 (mdc 225 25) )
testMdc09 = TestCase (assertEqual "testeMdc09: mdc entre 1 e -1" 1 (mdc 1 (-1) ) )
testMdc10 = TestCase (assertEqual "testeMdc11: mdc entre -1 e 1" 1 (mdc (-1) 1) ) -- Considerei que números negativos podem ser entrada da função, visto que o sinal negativo não afeta a divisibilidade (http://math.info/Arithmetic/GCF/)
testMdc11 = TestCase (assertEqual "testeMdc12: mdc entre -1 e -1" 1 (mdc (-1) (-1) ) )
testMdc12 = TestCase (assertEqual "testeMdc13: mdc entre -2 e -2" 2 (mdc (-2) (-2) ) )
testMdc13 = TestCase (assertEqual "testeMdc14: mdc entre -7 e -13" 1 (mdc (-7) (-13) ) )
testMdc14 = TestCase (assertEqual "testeMdc15: mdc entre -12 e -24" 12 (mdc (-12) (-24) ) )

testsMdc = TestList [ testMdc01, testMdc02, testMdc03, testMdc04, testMdc05,
  testMdc06, testMdc07, testMdc08, testMdc09, testMdc10, testMdc11, testMdc12,
  testMdc13, testMdc14 ]

{- Utilizei como base para os parâmetros dos testes a definição dos autores que defendem que 0 pode ser utilizado
como entrada para a função mmc (referência: https://en.wikipedia.org/wiki/Least_common_multiple )
 -}
testMmc01 = TestCase (assertEqual "testMmc01: mmc entre 0 e 0" 0 (mmc 0 0) )
testMmc02 = TestCase (assertEqual "testMmc02: mmc entre 0 e 1" 0 (mmc 0 1) )
testMmc03 = TestCase (assertEqual "testMmc03: mmc entre 1 e 0" 0 (mmc 1 0) )
testMmc04 = TestCase (assertEqual "testMmc04: mmc entre 1 e 1" 1 (mmc 1 1) )
testMmc05 = TestCase (assertEqual "testMmc05: mmc entre 1 e 2" 2 (mmc 1 2) )
testMmc06 = TestCase (assertEqual "testMmc06: mmc entre 3 e 3" 3 (mmc 3 3) )
testMmc07 = TestCase (assertEqual "testMmc07: mmc entre 4 e 2" 4 (mmc 4 2) )
testMmc08 = TestCase (assertEqual "testMmc08: mmc entre 3 e 7" 21 (mmc 3 7) )

testsMmc = TestList [ testMmc01, testMmc02, testMmc03, testMmc04, testMmc05, testMmc06,
   testMmc07, testMmc08 ]

testCoprimo01 = TestCase (assertEqual "testCoprimo01: 0 e 0 são coprimos?" False (coprimo 0 0) )
testCoprimo02 = TestCase (assertEqual "testCoprimo02: 0 e 1 são coprimos?" True (coprimo 0 1) )
testCoprimo03 = TestCase (assertEqual "testCoprimo03: 1 e 1 são coprimos?" True (coprimo 1 1) )
testCoprimo04 = TestCase (assertEqual "testCoprimo04: 2 e 1 são coprimos?" True (coprimo 2 1) )
testCoprimo05 = TestCase (assertEqual "testCoprimo05: 2 e 2 são coprimos?" False (coprimo 2 2) )
testCoprimo06 = TestCase (assertEqual "testCoprimo06: 5 e 15 são coprimos?" False (coprimo 5 15) )
testCoprimo07 = TestCase (assertEqual "testCoprimo07: 7 e 13 são coprimos?" True (coprimo 7 13) )
testCoprimo08 = TestCase (assertEqual "testCoprimo08: 225 e 25 são coprimos?" False (coprimo 225 25) )
testCoprimo09 = TestCase (assertEqual "testCoprimo09: 1 e -1 são coprimos?" True (coprimo 1 (-1) ) )
testCoprimo10 = TestCase (assertEqual "testCoprimo10: -1 e 1 são coprimos?" True (coprimo (-1) 1) )
testCoprimo11 = TestCase (assertEqual "testCoprimo11: -1 e -1 são coprimos?" True (coprimo (-1) (-1) ) )
testCoprimo12 = TestCase (assertEqual "testCoprimo12: -2 e -2 são coprimos?" False (coprimo (-2) (-2) ) )
testCoprimo13 = TestCase (assertEqual "testCoprimo13: -7 e -13 são coprimos?" True (coprimo (-7) (-13) ) )
testCoprimo14 = TestCase (assertEqual "testCoprimo14: -12 e -24 são coprimos?" False (coprimo (-12) (-24) ) )

testsCoprimo = TestList [ testCoprimo01, testCoprimo02, testCoprimo03,testCoprimo04,
  testCoprimo05,testCoprimo06, testCoprimo07, testCoprimo08, testCoprimo09,
  testCoprimo10, testCoprimo11, testCoprimo12, testCoprimo13, testCoprimo14 ]

testGoldbach01 = TestCase ( assertEqual "testGoldbach01: goldbach de 3 tem (1,2)"
  True (elem (1, 2) (goldbach 3)))
testGoldbach02 = TestCase ( assertEqual "testGoldbach02: goldbach de 3 não tem (0,3)"
  False (elem (0, 3) (goldbach 3)))
testGoldbach03 = TestCase ( assertEqual "testGoldbach03: goldbach de 4 tem (2,2)"
  True (elem (2, 2) (goldbach 4)))
testGoldbach04 = TestCase ( assertEqual "testGoldbach04: goldbach de 4 tem (1,3)"
  True (elem (1, 3) (goldbach 4)))
testGoldbach05 = TestCase ( assertEqual "testGoldbach05: goldbach de 28 não tem (5,24)"
  False (elem (5, 24) (goldbach 28)))
testGoldbach06 = TestCase ( assertEqual "testGoldbach06: goldbach de 28 tem (5,23)"
  True (elem (5, 23) (goldbach 28)))
testGoldbach07 = TestCase ( assertEqual "testGoldbach07: goldbach de 10 tem (3,7)"
  True (elem (3, 7) (goldbach 10)))
testGoldbach08 = TestCase ( assertEqual "testGoldbach08: goldbach de 100 tem (97,3)"
  True (elem (97, 3) (goldbach 100)))
testGoldbach09 = TestCase ( assertEqual "testGoldbach09: goldbach de 100 não tem (96,4)"
  False (elem (96, 4) (goldbach 100)))
testGoldbach10 = TestCase (assertException ForbiddenNumber (evaluate $  goldbach 2 ) ) -- Teste para caso excepcional (fora do domínio do problema) para goldbach igual a dois
testGoldbach11 = TestCase (assertException ForbiddenNumber (evaluate $  (goldbach (-1)) ) ) -- Teste para caso excepcional (fora do domínio do problema) para goldbach menor que dois

testsGoldbach = TestList [testGoldbach01, testGoldbach02, testGoldbach03, testGoldbach04,
  testGoldbach05, testGoldbach06, testGoldbach07, testGoldbach08, testGoldbach09,
  testGoldbach10, testGoldbach11 ]

testMeuLast01 = TestCase ( assertEqual "last de lista com um elemento" 1 (meuLast [1]) )
testMeuLast02 = TestCase ( assertEqual "last de lista com dois elementos" 2 (meuLast [1,2]) )
testMeuLast03 = TestCase ( assertEqual "last de lista com n elementos" 200 (meuLast [1..200]) )
testMeuLast04 = TestCase (assertException EmptyList (evaluate $ (meuLast []) ) ) -- Teste para caso excepcional (fora do domínio do problema) para last de uma lista vazia

testsMeuLast = TestList [testMeuLast01, testMeuLast02, testMeuLast03, testMeuLast04 ]

testPenultimo01 = TestCase ( assertEqual "testPenultimo01: penultimo de lista com 2" 1 (penultimo [1,2]) )
testPenultimo02 = TestCase ( assertEqual "testPenultimo02: for (penultimo [1,2,3,4,5,6])" 5 (penultimo [1,2,3,4,5,6]) )
testPenultimo03 = TestCase ( assertEqual "testPenultimo03: penultimo de lista com muitos elementos" 99 (penultimo [1,3..101]) )
testPenultimo04 = TestCase (assertException EmptyList (evaluate $  penultimo [1] ) ) -- Teste para caso excepcional (fora do domínio do problema) para penúltimo de uma lista com 1 elemento
testPenultimo05 = TestCase (assertException EmptyList (evaluate $  penultimo [] ) ) -- Teste para caso excepcional (fora do domínio do problema) para penúltimo de uma lista vazia

testsPenultimo = TestList [ testPenultimo01, testPenultimo02, testPenultimo03,
  testPenultimo04, testPenultimo05 ]

testElementAt01 = TestCase ( assertEqual "testElementAt01: elemento na pos 1 de uma lista com tamanho 1" 1 (elementAt 1 [1]))
testElementAt02 = TestCase ( assertEqual "testElementAt02: elemento pos 1 de uma lista de tamanho n" 1 (elementAt 1 [1..10]))
testElementAt03 = TestCase ( assertEqual "testElementAt01: elemento na pos 10 de uma lista de tamanho 10" 10 (elementAt 10 [1..10]))
testElementAt04 = TestCase ( assertEqual "testElementAt03: elemento posicionado no meio da lista" 10 (elementAt 10 [1..20]))
testElementAt05 = TestCase (assertException EmptyList (evaluate $  elementAt 4 [1,2,3]) ) -- Teste para caso excepcional (fora do domínio do problema) para elementAt com index fora da lista

testsElementAt = TestList [ testElementAt01, testElementAt02, testElementAt03, testElementAt04, testElementAt05 ]

testMeuLength01 = TestCase ( assertEqual "testMeuLength01: lista vazia" 0 (meuLength []) )
testMeuLength02 = TestCase ( assertEqual "testMeuLength02: lista com um elemento" 1 (meuLength [1]) )
testMeuLength03 = TestCase ( assertEqual "testMeuLength03: lista com 5 elementos" 5 (meuLength [1,2,3,4,5]))
testMeuLength04 = TestCase ( assertEqual "testMeuLength04: lista com 50 elementos" 50 (meuLength [1..50]))
testMeuLength05 = TestCase ( assertEqual "testMeuLength05: teste com string" 3 (meuLength "abc") )

testsMeuLength = TestList [ testMeuLength01, testMeuLength02, testMeuLength03, testMeuLength04, testMeuLength05 ]

testMeuReverso01 = TestCase ( assertEqual "testMeuReverso01: lista vazia" [] ( meuReverso ([] :: [Int]) ) )
testMeuReverso02 = TestCase ( assertEqual "testMeuReverso02: lista vazia" ['a'] (meuReverso ['a']) )
testMeuReverso03 = TestCase ( assertEqual "testMeuReverso03: lista de inteiros" [3,2,1] (meuReverso [1,2,3]))
testMeuReverso04 = TestCase ( assertEqual "testMeuReverso04: lista de booleanos" [True,True,False] (meuReverso [False,True,True]))
testMeuReverso05 = TestCase ( assertEqual "testMeuReverso05: lista com muitos elementos" [50,49..0] (meuReverso [0,1..50]))

testsMeuReverso = TestList [ testMeuReverso01, testMeuReverso02, testMeuReverso03, testMeuReverso04, testMeuReverso05 ]

testIsPalindrome01 = TestCase (assertEqual "testIsPalindrome01: for isPalindrome asa " True (isPalindrome "asa"))
testIsPalindrome02 = TestCase (assertEqual "testIsPalindrome02: for isPalindrome [1,0,1]" True (isPalindrome [1,0,1]))
testIsPalindrome03 = TestCase (assertEqual "testIsPalindrome03: for isPalindrome casa " False (isPalindrome "casa"))
testIsPalindrome04 = TestCase (assertEqual "testIsPalindrome04: for isPalindrome " True (isPalindrome "") )

testsIsPalindrome = TestList [ testIsPalindrome01, testIsPalindrome02, testIsPalindrome03,testIsPalindrome04 ]

testCompress01 = TestCase ( assertEqual "testCompress01: for compress []" [] (compress ([] :: [Int])) )
testCompress02 = TestCase ( assertEqual "testCompress02: for compress [1]" [1] (compress [1]) )
testCompress03 = TestCase ( assertEqual "testCompress03: for compress [6,6,6]" [6] (compress [6,6,6]) )
testCompress04 = TestCase ( assertEqual "testCompress04: for compress [2,5,8,2,1,8]" [2,5,8,1] (compress [2,5,8,2,1,8]) )
testCompress05 = TestCase ( assertEqual "testCompress05: for compress [2,5,2,1,8,8]" [2,5,1,8] (compress [2,5,2,1,8,8]) )
testCompress06 = TestCase ( assertEqual "testCompress06: for compress [1,5..300]" [1,5..300] (compress [1,5..300]) )
testCompress07 = TestCase ( assertEqual "testCompress07: for compress alakazan" "alkzn" (compress "alakazan") )

testsCompress = TestList [ testCompress01, testCompress02, testCompress03, testCompress04,
  testCompress05, testCompress06, testCompress07 ]

testCompact01 = TestCase ( assertEqual "testCompact01: for compact []" [] (compact ([] :: [Int]) ) )
testCompact02 = TestCase ( assertEqual "testCompact02: for compact [2]" [2] (compact [2]) )
testCompact03 = TestCase ( assertEqual "testCompact03: for compact [2,5,8,2,1,8]" [2,2,5,8,8,1] (compact [2,5,8,2,1,8]) )
testCompact04 = TestCase ( assertEqual "testCompact04: for compact [0,1,0,1,0,1,0,1]" [0,0,0,0,1,1,1,1] (compact  [0,1,0,1,0,1,0,1]) )
testCompact05 = TestCase ( assertEqual "testCompact05: for compact [1..50]" [1..50] (compact [1..50]) )

testsCompact = TestList [ testCompact01, testCompact02, testCompact03, testCompact04, testCompact05 ]

testEncode01 = TestCase ( assertEqual "testEncode01: for encode []"
  [] (LambdaCalculus.encode ([] :: [Int]) ) )
testEncode02 = TestCase ( assertEqual "testEncode02: for encode [1]"
  [(1,1)] (LambdaCalculus.encode [1]) )
testEncode03 = TestCase ( assertEqual "testEncode03: for encode [1,2]"
  [(1,1), (2,1)] (LambdaCalculus.encode [1,2]) )
testEncode04 = TestCase ( assertEqual "testEncode04: for encode [1,2,0,3,2,1]"
  [(1,2), (2,2), (0,1), (3,1)] (LambdaCalculus.encode [1,2,0,3,2,1]) )
testEncode05 = TestCase ( assertEqual "testEncode04: for encode [1,2,0,3,2,1]"
  [('a',1), ('n',2), ('d',1), ('e',1), ('r',1), ('s',1), ('o',1)] (LambdaCalculus.encode "anderson") )

testsEncode = TestList [ testEncode01, testEncode02, testEncode03, testEncode04, testEncode05 ]

testSplit01 = TestCase ( assertEqual "testSplit01: for split 1 [0,9,9,9,9,9]" [[0], [9,9,9,9,9]] (LambdaCalculus.split 1 [0,9,9,9,9,9]) )
testSplit02 = TestCase ( assertEqual "testSplit02: for split 0 [0,9,9,9,9,9]" [[], [0,9,9,9,9,9]] (LambdaCalculus.split 0 [0,9,9,9,9,9]) )
testSplit03 = TestCase ( assertEqual "testSplit03: for split 3 [0,0,0,9,9,9,9,9]" [[0,0,0], [9,9,9,9,9]] (LambdaCalculus.split 3 [0,0,0,9,9,9,9,9]) )
testSplit04 = TestCase ( assertEqual "testSplit04: for split 1 [1]" [[1], []] (LambdaCalculus.split 1 [1]) )
testSplit05 = TestCase ( assertEqual "testSplit05: for split 0 [1]" [[], [1]] (LambdaCalculus.split 0 [1]) )
testSplit06 = TestCase ( assertEqual "testSplit06: for split star wars" ["star", " wars"] (LambdaCalculus.split 4 "star wars") )

testsSplit = TestList [ testSplit01, testSplit02, testSplit03, testSplit04, testSplit05, testSplit06 ]

testSlice01 = TestCase ( assertEqual "testSlice01: for slice [1,2,3,4,5,6,7,8,9] 1 9" [1,2,3,4,5,6,7,8,9] (slice [1,2,3,4,5,6,7,8,9] 1 9) )
testSlice02 = TestCase ( assertEqual "testSlice02: for slice [1,2,3,4,5,6,7,8,9] 2 9" [2,3,4,5,6,7,8,9] (slice [1,2,3,4,5,6,7,8,9] 2 9) )
testSlice03 = TestCase ( assertEqual "testSlice03: for slice [1,2,3,4,5,6,7,8,9] 1 8" [1,2,3,4,5,6,7,8] (slice [1,2,3,4,5,6,7,8,9] 1 8) )
testSlice04 = TestCase ( assertEqual "testSlice04: for slice [1,2,3,4,5,6,7,8,9] 3 6" [3,4,5,6] (slice [1,2,3,4,5,6,7,8,9] 3 6) )
testSlice05 = TestCase ( assertEqual "testSlice05: for slice [1] 1 1" [1] (slice [1] 1 1) )
testSlice06 = TestCase ( assertEqual "testSlice06: for slice [1,2,3,4,5,6,7,8,9] 1 0" [] (slice [1,2,3,4,5,6,7,8,9] 1 0) )
testSlice07 = TestCase ( assertEqual "testSlice07: for slice lambda 1 4" "lamb" (slice "lambda" 1 4) )

testsSlice = TestList [ testSlice01, testSlice02, testSlice03, testSlice04, testSlice05, testSlice06, testSlice07 ]

testInsertAt01 = TestCase ( assertEqual "testInsertAt01: for insertAt 5 1 [0,0,0,0]" [5,0,0,0,0] (insertAt 5 1 [0,0,0,0]))
testInsertAt02 = TestCase ( assertEqual "testInsertAt02: for insertAt 5 5 [0,0,0,0]" [0,0,0,0,5] (insertAt 5 5 [0,0,0,0]))
testInsertAt03 = TestCase ( assertEqual "testInsertAt03: for insertAt 5 4 [0,0,0,0]" [0,0,0,5,0] (insertAt 5 4 [0,0,0,0]))
testInsertAt04 = TestCase ( assertEqual "testInsertAt04: for insertAt 5 4 [0,0,0,0]" [5] (insertAt 5 1 []))
testInsertAt05 = TestCase ( assertEqual "testInsertAt05: for insertAt r 3 copo" "corpo" (insertAt 'r' 3 "copo"))

testsInsertAt = TestList [ testInsertAt01, testInsertAt02, testInsertAt03, testInsertAt04, testInsertAt05 ]

testSort01 = TestCase ( assertEqual "testSort01: lista vazia" [] (sort ([] :: [Int]) ) )
testSort02 = TestCase ( assertEqual "testSort02: lista com um elemento" [1] (sort [1]) )
testSort03 = TestCase ( assertEqual "testSort03: lista de elementos iguais" [1,1,1] (sort [1,1,1]) )
testSort04 = TestCase ( assertEqual "testSort04: lista ordenada" [1,2,3] (sort [1,2,3]) )
testSort05 = TestCase ( assertEqual "testSort05: for (sort [2,2,1,3,0])" [0,1,2,2,3] (sort [2,2,1,3,0]) )
testSort06 = TestCase ( assertEqual "testSort06: lista com elemento negativo" [(-2),(-1),0,1,3] (sort [1,(-2),3, 0,(-1)]) )

testsSort = TestList [ testSort01, testSort02, testSort03, testSort04, testSort05, testSort06 ]

testMySum01 = TestCase (assertEqual "testMySum01: for mySum [5,5,5,5,5]" 25 (mySum [5,5,5,5,5]))
testMySum02 = TestCase (assertEqual "testMySum02: soma de lista com um elemento é o próprio elemento" 5 (mySum [5]))
testMySum03 = TestCase (assertEqual "testMySum03: soma de lista vazia" 0 (mySum []))

testsMySum = TestList [ testMySum01, testMySum02, testMySum03 ]

testMaxList01 = TestCase (assertEqual "testMaxList01: for maxList [1]" 1 (maxList [1]) )
testMaxList02 = TestCase (assertEqual "testMaxList02: for maxList [1,5]" 5 (maxList [1, 5]) )
testMaxList03 = TestCase (assertEqual "testMaxList03: for maxList [5,1]" 5 (maxList [5, 1]) )
testMaxList04 = TestCase (assertEqual "testMaxList04: for maxList [1..100]" 100 (maxList [1..100]) )
testMaxList05 = TestCase (assertException EmptyList (evaluate $  maxList [] ) ) -- Teste para caso excepcional (fora do domínio do problema) para maxList de uma lista vazia

testsMaxList = TestList [ testMaxList01,testMaxList02, testMaxList03, testMaxList04, testMaxList05 ]

testBuildPalindrome01 = TestCase (assertEqual "testBuildPalindrome01: for buildPalindrome []" True (Prelude.null (buildPalindrome []))  )
testBuildPalindrome02 = TestCase (assertEqual "testBuildPalindrome02: for buildPalindrome [1]" [1,1] (buildPalindrome [1]))
testBuildPalindrome03 = TestCase (assertEqual "testBuildPalindrome03: for buildPalindrome [1,2,3]" [1,2,3,3,2,1] (buildPalindrome [1,2,3]))
testBuildPalindrome04 = TestCase (assertEqual "testBuildPalindrome04: for buildPalindrome os" "osso" (buildPalindrome "os"))

testsBuildPalindrome = TestList [ testBuildPalindrome01, testBuildPalindrome02, testBuildPalindrome03, testBuildPalindrome04 ]

testMean01 = TestCase (assertEqual "testMean01: for mean [1]" 1.0 (mean [1]) )
testMean02 = TestCase (assertEqual "testMean02: for mean [0,0,0,0]" 0.0 (mean [0,0,0,0]) )
testMean03 = TestCase (assertEqual "testMean03: for mean [1,3..100]" 50.0 (mean [1,3..100]) )
testMean04 = TestCase (assertEqual "testMean04: for mean [2,3]" 2.5 (mean [2,3]) )
testMean05 = TestCase (assertEqual "testMean05: for mean [50..600]" 325.0 (mean [50..600]) )
testMean06 = TestCase (assertException EmptyList (evaluate $  mean [] ) ) -- Teste para caso excepcional (fora do domínio do problema) para mean de uma lista vazia

testsMean = TestList [ testMean01, testMean02, testMean03, testMean04, testMean05, testMean06 ]

testMyAppend01 = TestCase (assertEqual "testMyAppend01: for myAppend [] []" True (Prelude.null (myAppend [] [])) )
testMyAppend02 = TestCase (assertEqual "testMyAppend02: for myAppend [1] []" [1] (myAppend [1] []) )
testMyAppend03 = TestCase (assertEqual "testMyAppend03: for myAppend [] [1]" [1] (myAppend [] [1]) )
testMyAppend04 = TestCase (assertEqual "testMyAppend04: for myAppend [1,2,3] [4,5,6]" [1] (myAppend [1] []) )
testMyAppend05 = TestCase (assertEqual "testMyAppend05: for myAppend haskell +  é top" "haskell é top" (myAppend "haskell" " é top") )

testsMyAppend = TestList [ testMyAppend01, testMyAppend02, testMyAppend03, testMyAppend04 ]

-- Lista com todos os testes do módulo
tests = TestList [ testsPow, testsFatorial, testsIsPrime, testsFib, testsMdc, testsMmc,
  testsCoprimo, testsGoldbach, testsMeuLast, testsPenultimo, testsElementAt,
  testsMeuLength, testsMeuReverso, testsIsPalindrome, testsCompress, testsCompact,
  testsEncode, testsSplit, testsSlice, testsInsertAt, testsSort, testsMySum,
  testsMaxList, testsBuildPalindrome, testsMean, testsMyAppend ]

  {- Método para assegurar que a chamada de função lança uma determinada
  exceção, gerando uma falha caso nenhuma exceção seja lançada.
  -}
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
    action
    assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

{- Função utilizada para gerar o valor PutText a ser usado na execução dos testes -}
myPutText = PutText reportMsg 0  :: PutText Int

{- Função auxiliar que gera um contador para os casos de teste -}
reportMsg :: String -> Bool -> Int -> IO Int
reportMsg message isProgress count = do
  return (count + 1)


{- Cria uma instância para representar no formato JSON o report de erros do
aluno, contendo: matricula, total de testes, testes que passaram, testes que
falharam e testes que geraram erro não esperado -}
instance ToJSON Counts where
  toJSON (Counts cases tried errors failures) = object
    [ pack "matricula" .= ""
    , pack "totalTestes" .= show tried
    , pack "erros" .= show errors
    , pack "falhas" .= show failures
    , pack "passaram" .= show (tried - errors - failures)
    ]

{- Roda os testes e exibe na tela o relatório de cada erro que aconteceu. Armazena em
um arquivo JSON a amostragem coletada dos resultados dos testes, como definido na
instância de toJSON Counts.
-}
main = do
  runTestTT tests
  (testCounts, msgCount) <- runTestText myPutText tests
  let nomeArquivo = "test-output.json"
  I.writeFile nomeArquivo (encodeToLazyText testCounts)
  Prelude.putStrLn ("Arquivo json criado: " ++ nomeArquivo)
