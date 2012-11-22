import Test.HUnit

import Aufgabe6

-- converts an integer to Nat
decToNat :: Integer -> Nat
decToNat 0 = Z
decToNat i = S (decToNat (i-1))


-- tests cases for Aufgabe 6.1
test8 = TestCase (assertEqual "" "VIII" (show (decToNat 8)) )
test9 = TestCase (assertEqual "" "IX" (show (decToNat 9)))
test19 = TestCase (assertEqual "" "XIX" (show (decToNat 19)))
test39 = TestCase (assertEqual "" "XXXIX"(show (decToNat 39)))
test84 = TestCase (assertEqual "" "LXXXIV" (show (decToNat 84)))
test89 = TestCase (assertEqual "" "LXXXIX" (show (decToNat 89)))
test98 = TestCase (assertEqual "" "XCVIII" (show (decToNat 98)))
test99 = TestCase (assertEqual "" "IC" (show (decToNat 99)))
test889 = TestCase (assertEqual "" "DCCCLXXXIX" (show (decToNat 889)))
test1899 = TestCase (assertEqual "" "MDCCCIC" (show (decToNat 1899)))
test4884 = TestCase (assertEqual "" "MMMMDCCCLXXXIV" (show (decToNat 4884)))
test5999 = TestCase (assertEqual "" "MMMMMIM" (show (decToNat 5999)))

tests = TestList [TestLabel "Test 8" test8, TestLabel "Test 9" test9,
                  TestLabel "Test 19" test19, TestLabel "Test 39" test39,
                  TestLabel "Test 84" test84, TestLabel "Test89" test89,
                  TestLabel "Test 98" test98, TestLabel "Test 99" test99,
                  TestLabel "Test 889" test889, TestLabel "Test 1899" test1899,
                  TestLabel "Test 4884" test4884, TestLabel "Test 5999" test5999]

main = runTestTT tests
