{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Control.Monad (liftM3, forM)

import Aufgabe8


test1 = TestCase (assertEqual "" (Success (4,9339,"9339")) (addRev 195 100))
test2 = TestCase (assertEqual "" (Success (4,9339,"9339")) (addRev (-195) (-10)))
test3 = TestCase (assertEqual "" Failure (addRev 195 3))
test4 = TestCase (assertEqual "" (Success (0,888,"888")) (addRev 888 0))
test5 = TestCase (assertEqual "" (Success (0,989,"989")) (addRev 989 10))
test6 = TestCase (assertEqual "" (Success (3,6666,"6666")) (addRev 750 (-5)))
test7 = TestCase (assertEqual "" Failure (addRev 196 1000))


connections = [(Air "Austria" "Germany" 2),
               (Rail "Austria" "Germany" 10),
               (Air "Germany" "Spain" 3),
               (Sea "Spain" "UK" 15),
               (Road "Canada" "US" 5)
  ]

test10 = TestCase (assertEqual "" True (isRoute connections "Austria" "Spain"))
test11 = TestCase (assertEqual "" True (isRoute connections "Austria" "UK"))
test12 = TestCase (assertEqual "" False (isRoute connections "US" "UK"))

tests = TestList [TestLabel "Test 1" test1, TestLabel "Test 2" test2,
                  TestLabel "Test 3" test3, TestLabel "Test 4" test4,
                  TestLabel "Test 5" test5, TestLabel "Test 6" test6,
                  TestLabel "Test 7" test7,
                  TestLabel "isRoute" test10, TestLabel "isRoute" test11,
                  TestLabel "isRoute" test12]


main = do
  runTestTT tests
