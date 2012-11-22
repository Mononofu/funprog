import Test.HUnit

import Aufgabe6

-- converts an integer to Nat
decToNat :: Integer -> Nat -> Nat
decToNat 0 val = val
decToNat i val = decToNat (i-1) (S val)


-- tests cases for Aufgabe 6.1
test8 = TestCase (assertEqual "" "VIII" (show (decToNat 8 Z)) )
test9 = TestCase (assertEqual "" "IX" (show (decToNat 9 Z)))
test19 = TestCase (assertEqual "" "XIX" (show (decToNat 19 Z)))
test39 = TestCase (assertEqual "" "XXXIX"(show (decToNat 39 Z)))
test84 = TestCase (assertEqual "" "LXXXIV" (show (decToNat 84 Z)))
test89 = TestCase (assertEqual "" "LXXXIX" (show (decToNat 89 Z)))
test98 = TestCase (assertEqual "" "XCVIII" (show (decToNat 98 Z)))
test99 = TestCase (assertEqual "" "IC" (show (decToNat 99 Z)))
test889 = TestCase (assertEqual "" "DCCCLXXXIX" (show (decToNat 889 Z)))
test1899 = TestCase (assertEqual "" "MDCCCIC" (show (decToNat 1899 Z)))
test4884 = TestCase (assertEqual "" "MMMMDCCCLXXXIV" (show (decToNat 4884 Z)))
test5999 = TestCase (assertEqual "" "MMMMMIM" (show (decToNat 5999 Z)))

tests = TestList [TestLabel "Test 8" test8, TestLabel "Test 9" test9,
                  TestLabel "Test 19" test19, TestLabel "Test 39" test39,
                  TestLabel "Test 84" test84, TestLabel "Test89" test89,
                  TestLabel "Test 98" test98, TestLabel "Test 99" test99,
                  TestLabel "Test 889" test889, TestLabel "Test 1899" test1899,
                  TestLabel "Test 4884" test4884, TestLabel "Test 5999" test5999]

main = runTestTT tests
