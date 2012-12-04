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

tests = TestList [TestLabel "Test 1" test1, TestLabel "Test 2" test2,
                  TestLabel "Test 3" test3, TestLabel "Test 4" test4,
                  TestLabel "Test 5" test5, TestLabel "Test 6" test6,
                  TestLabel "rm_dup" test7]


main = do
  runTestTT tests
