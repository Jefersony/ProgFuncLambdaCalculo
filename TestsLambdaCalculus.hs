module TestsLambdaCalculus where
import LambdaCalculus
import Data.Function
import Data.Aeson
import Data.Text
import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Text (encodeToLazyText)
import Test.HUnit
import Control.Exception.Base

testPow01 = TestCase (assertEqual "testPow01: 0 elevado a 0" 1 (pow 0 0) )
testPow02 = TestCase (assertEqual "testPow02: 1 elevado a 0" 1 (pow 1 0) )
testPow03 = TestCase (assertEqual "testPow03: 0 elevado a 1" 0 (pow 0 1) )
testPow04 = TestCase (assertEqual "testPow04: 0 elevado a 2" 0 (pow 0 2) )
testPow05 = TestCase (assertEqual "testPow05: 5 elevado a 1" 5 (pow 5 1) )
testPow06 = TestCase (assertEqual "testPow06: 3 elevado a 2" 9 (pow 3 2) )

testsPow = TestList [testPow01, testPow02, testPow03, testPow04, testPow05,
  testPow06]

testFatorial01 = TestCase (assertEqual "testFatorial01: fatorial de 0" 1 (fatorial 0) )
testFatorial02 = TestCase (assertEqual "testFatorial02: fatorial de 1" 1 (fatorial 1) )
testFatorial03 = TestCase (assertEqual "testFatorial03: fatorial de 2" 2 (fatorial 2) )
testFatorial04 = TestCase (assertEqual "testFatorial04: fatorial de 3" 6 (fatorial 3) )
testFatorial05 = TestCase (assertEqual "testFatorial05: fatorial de 4" 24 (fatorial 4) )
testFatorial06 = TestCase (assertEqual "testFatorial06: fatorial de 10" 3628800 (fatorial 10) )

testsFatorial = TestList [testFatorial01, testFatorial02, testFatorial03,
  testFatorial04, testFatorial05, testFatorial06]

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

testsFib = TestList [testFib01, testFib02, testFib03, testFib04, testFib05,
  testFib06,testFib07,testFib08]

testMdc01 = TestCase (assertEqual "testeMdc01: mdc entre 0 e 0" 0 (mdc 0 0) )
testMdc02 = TestCase (assertEqual "testeMdc02: mdc entre 0 e 1" 1 (mdc 0 1) )
testMdc03 = TestCase (assertEqual "testeMdc03: mdc entre 1 e 1" 1 (mdc 1 1) )
testMdc04 = TestCase (assertEqual "testeMdc04: mdc entre 2 e 1" 1 (mdc 2 1) )
testMdc05 = TestCase (assertEqual "testeMdc05: mdc entre 2 e 2" 2 (mdc 2 2) )
testMdc06 = TestCase (assertEqual "testeMdc06: mdc entre 5 e 15" 5 (mdc 5 15) )
testMdc07 = TestCase (assertEqual "testeMdc07: mdc entre 7 e 13" 1 (mdc 7 13) )
testMdc08 = TestCase (assertEqual "testeMdc08: mdc entre 225 e 25" 25 (mdc 225 25) )
testMdc09 = TestCase (assertEqual "testeMdc09: mdc entre 1 e -1" 1 (mdc 1 (-1) ) )
testMdc10 = TestCase (assertEqual "testeMdc11: mdc entre -1 e 1" 1 (mdc (-1) 1) )
testMdc11 = TestCase (assertEqual "testeMdc12: mdc entre -1 e -1" 1 (mdc (-1) (-1) ) )
testMdc12 = TestCase (assertEqual "testeMdc13: mdc entre -2 e -2" 2 (mdc (-2) (-2) ) )
testMdc13 = TestCase (assertEqual "testeMdc14: mdc entre -7 e -13" 1 (mdc (-7) (-13) ) )
testMdc14 = TestCase (assertEqual "testeMdc15: mdc entre -12 e -24" 12 (mdc (-12) (-24) ) )

testsMdc = TestList [testMdc01, testMdc02, testMdc03, testMdc04, testMdc05,
  testMdc06, testMdc07, testMdc08, testMdc09, testMdc10, testMdc11, testMdc12,
  testMdc13, testMdc14]

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

testsCoprimo = TestList [testCoprimo01, testCoprimo02, testCoprimo03,testCoprimo04,
  testCoprimo05,testCoprimo06, testCoprimo07, testCoprimo08, testCoprimo09,
  testCoprimo10, testCoprimo11, testCoprimo12, testCoprimo13, testCoprimo14]

testGoldbach01 = TestCase ( assertEqual "testGoldbach01" False (elem (5, 24) (goldbach 28)))
testGoldbach02 = TestCase ( assertEqual "testGoldbach02" True (elem (5, 23) (goldbach 28)))
testGoldbach03 = TestCase ( assertEqual "testGoldbach03" True (elem (3, 7) (goldbach 10)))
testGoldbach04 = TestCase ( assertEqual "testGoldbach04" True (elem (1, 3) (goldbach 4)))
testGoldbach05 = TestCase ( assertEqual "testGoldbach05" True (elem (97, 3) (goldbach 100)))
testGoldbach06 = TestCase ( assertEqual "testGoldbach06" False (elem (96, 4) (goldbach 100)))

testsGoldbach = TestList [testGoldbach01, testGoldbach02, testGoldbach03, testGoldbach04,
  testGoldbach05, testGoldbach06]

testMeuLast01 = TestCase ( assertEqual "last de lista com um elemento" 1 (meuLast [1]) )
testMeuLast02 = TestCase ( assertEqual "last de lista com dois elementos" 2 (meuLast [1,2]) )
testMeuLast03 = TestCase ( assertEqual "last de lista com n elementos" 200 (meuLast [1..200]) )
-- testMeuLast04 = TestCase ( assertEqual "testMeuLast04" "Empty list" (meuLast []) )

testsMeuLast = TestList [testMeuLast01, testMeuLast02, testMeuLast03 {-, testMeuLast04-}]

testPenultimo01 = TestCase ( assertEqual "testPenultimo01" 1 (penultimo [1,2]) )
testPenultimo02 = TestCase ( assertEqual "testPenultimo02" 5 (penultimo [1,2,3,4,5,6]) )
testPenultimo03 = TestCase ( assertEqual "testPenultimo03" 99 (penultimo [1,3..101]) )

testsPenultimo = TestList [ testPenultimo01, testPenultimo02, testPenultimo03]

testElementAt01 = TestCase ( assertEqual "testElementAt01" 9 (elementAt 9 [1..10]))
testElementAt02 = TestCase ( assertEqual "testElementAt02" 1 (elementAt 1 [1..10]))
testElementAt03 = TestCase ( assertEqual "testElementAt03" 10 (elementAt 10 [1..20]))

testsElementat = TestList [ testElementAt03, testElementAt02, testElementAt01]

testMeuLength01 = TestCase ( assertEqual "testMeuLength01" 5 (meuLength [1,2,3,4,5]))
testMeuLength02 = TestCase ( assertEqual "testMeuLength02" 50 (meuLength [1..50]))
testMeuLength03 = TestCase ( assertEqual "testMeuLength03" 25 (meuLength [1,3..50]))

testsMeuLength = TestList [ testMeuLength01, testMeuLength02, testMeuLength03]

testMeuReverso01 = TestCase ( assertEqual "testMeuReverso01" [3,2,1] (meuReverso [1,2,3]))
testMeuReverso02 = TestCase ( assertEqual "testMeuReverso02" [1,2,3] (meuReverso [3,2,1]))
testMeuReverso03 = TestCase ( assertEqual "testMeuReverso03" [50,49..0] (meuReverso [0,1..50]))
testMeuReverso04 = TestCase ( assertEqual "testMeuReverso04" [7] (meuReverso [7]))

testsMeuReverso = TestList [ testMeuReverso01, testMeuReverso02, testMeuReverso03, testMeuReverso04 ]

testIsPalindrome01 = TestCase (assertEqual "testIsPalindrome01" True (isPalindrome "asa"))
testIsPalindrome02 = TestCase (assertEqual "testIsPalindrome02" True (isPalindrome [1,0,1]))
testIsPalindrome03 = TestCase (assertEqual "testIsPalindrome03" False (isPalindrome "casa"))
testIsPalindrome04 = TestCase (assertEqual "testIsPalindrome03" True (isPalindrome ""))

testsIsPalindrome = TestList [ testIsPalindrome01, testIsPalindrome02, testIsPalindrome03]

testCompress01 = TestCase ( assertEqual "testCompress01" [2,5,8,1] (compress [2,5,8,2,1,8]))
testCompress02 = TestCase ( assertEqual "testCompress02" [2,5,1,8] (compress [2,5,2,1,8,8]))
testCompress03 = TestCase ( assertEqual "testCompress03" [6] (compress [6,6,6]))
testCompress04 = TestCase ( assertEqual "testCompress04" [1,5..300] (compress [1,5..300]))

testsCompress = TestList [ testCompress01, testCompress02, testCompress03, testCompress04]

testCompact01 = TestCase ( assertEqual "testCompact01" [2,2,5,8,8,1] (compact [2,5,8,2,1,8]))
testCompact02 = TestCase ( assertEqual "testCompact02" [2] (compact [2]))
testCompact03 = TestCase ( assertEqual "testCompact03" [0,0,0,0,1,1,1,1] (compact  [0,1,0,1,0,1,0,1]))
--testCompact04 = TestCase ( assertEqual "testCompact04" [] (compact []))
testCompact05 = TestCase ( assertEqual "testCompact05" [1..50] (compact [1..50]))

testsCompact = TestList [ testCompact01,testCompact02,testCompact03,testCompact05]

{- Falta implementar a func encode -}
testsEncode = TestList []

testSplit01 = TestCase ( assertEqual "testSplit01" [[0], [9,9,9,9,9]] (LambdaCalculus.split 1 [0,9,9,9,9,9]))
testSplit02 = TestCase ( assertEqual "testSplit02" [[], [0,9,9,9,9,9]] (LambdaCalculus.split 0 [0,9,9,9,9,9]))
testSplit03 = TestCase ( assertEqual "testSplit03" [[0,0,0], [9,9,9,9,9]] (LambdaCalculus.split 3 [0,0,0,9,9,9,9,9]))
testSplit04 = TestCase ( assertEqual "testSplit04" [[1], []] (LambdaCalculus.split 1 [1]))
testSplit05 = TestCase ( assertEqual "testSplit05" [[], [1]] (LambdaCalculus.split 0 [1]))

testsSplit = TestList [ testSplit01, testSplit02, testSplit03, testSplit04, testSplit05 ]

testSlice01 = TestCase ( assertEqual "testSlice01" [1,2,3,4,5,6,7,8,9] (slice [1,2,3,4,5,6,7,8,9] 1 9))
testSlice02 = TestCase ( assertEqual "testSlice02" [2,3,4,5,6,7,8,9] (slice [1,2,3,4,5,6,7,8,9] 2 9))
testSlice03 = TestCase ( assertEqual "testSlice03" [1,2,3,4,5,6,7,8] (slice [1,2,3,4,5,6,7,8,9] 1 8))
testSlice04 = TestCase ( assertEqual "testSlice04" [3,4,5,6] (slice [1,2,3,4,5,6,7,8,9] 3 6))
testSlice05 = TestCase ( assertEqual "testSlice05" [1] (slice [1] 1 1))
testSlice06 = TestCase ( assertEqual "testSlice06" [] (slice [1,2,3,4,5,6,7,8,9] 1 0))

testsSlice = TestList [testSlice01, testSlice02, testSlice03, testSlice04, testSlice05, testSlice06]

testInsertAt01 = TestCase ( assertEqual "testInsertAt01" [5,0,0,0,0] (insertAt 5 1 [0,0,0,0]))
testInsertAt02 = TestCase ( assertEqual "testInsertAt02" [0,0,0,0,5] (insertAt 5 5 [0,0,0,0]))
testInsertAt03 = TestCase ( assertEqual "testInsertAt03" [0,0,0,5,0] (insertAt 5 4 [0,0,0,0]))
testInsertAt04 = TestCase ( assertEqual "testInsertAt04" [5] (insertAt 5 1 []))

testsInsertAt = TestList [testInsertAt01, testInsertAt02, testInsertAt03, testInsertAt04]

{- falta implementar func sort -}
testsSort = TestList []

testMySum01 = TestCase (assertEqual "testMySum01" 25 (mySum [5,5,5,5,5]))
testMySum02 = TestCase (assertEqual "testMySum02" 5 (mySum [5]))
testMySum03 = TestCase (assertEqual "testMySum03" 0 (mySum []))

testsMySum = TestList [testMySum01, testMySum02, testMySum03]

tests = TestList [testsPow, testsFatorial, testsIsPrime, testsFib, testsMdc,
  testsCoprimo, testsGoldbach, testsMeuLast, testsPenultimo, testsElementat,
  testsMeuLength, testsMeuReverso, testsIsPalindrome, testsCompress, testsCompact,
  testsEncode, testsSplit, testsSlice, testsInsertAt, testsSort, testsMySum]

reportMsg :: String -> Bool -> Int -> IO Int
reportMsg message isProgress count = do
  return (count + 1)

myPutText = PutText reportMsg 0  :: PutText Int

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

main = do
  runTestTT tests
  (testCounts, msgCount) <- runTestText myPutText tests
  I.writeFile "test-output.json" (encodeToLazyText testCounts)
  Prelude.putStrLn "Arquivo json criado (test-output.json) "
  return ()
