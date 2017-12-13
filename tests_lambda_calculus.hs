import Lambda_calculus
import Data.Function
import Test.HUnit


testPow01 = TestCase (assertEqual "testPow01" 1 (pow 0 0))
testPow02 = TestCase (assertEqual "testPow02" 1 (pow 1 0))
testPow03 = TestCase (assertEqual "testPow03" 0 (pow 0 1))
testPow04 = TestCase (assertEqual "testPow04" 0 (pow 0 2))
testPow05 = TestCase (assertEqual "testPow05" 5 (pow 5 1))
testPow06 = TestCase (assertEqual "testPow06" 9 (pow 3 2))

testsPow = TestList [testPow01, testPow02, testPow03, testPow04, testPow05, testPow06]

testsFatorial = TestList []
testsIsPrime = TestList []
testsFib = TestList []
testsMdc = TestList []
testsCoprimo = TestList []
testsGoldbach = TestList []
testsMeuLast = TestList []
testsPenultimo = TestList []
testsElementat = TestList []
testsMeuLength = TestList []
testsMeuReverso = TestList []
testsIsPalindrome = TestList []
testsCompress = TestList []
testsCompact = TestList []
testsEncode = TestList []
testsSplit = TestList []
testsSlice = TestList []
testsInsertAt = TestList []
testsSort = TestList []
testsMySum = TestList []

tests = TestList [testsPow, testsFatorial, testsIsPrime, testsFib, testsMdc,
  testsCoprimo, testsGoldbach, testsMeuLast, testsPenultimo, testsElementat,
  testsMeuLength, testsMeuReverso, testsIsPalindrome, testsCompress, testsCompact,
  testsEncode, testsSplit, testsSlice, testsInsertAt, testsSort, testsMySum]
