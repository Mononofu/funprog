import Test.HUnit

import Aufgabe7

test1 = TestCase (assertEqual "" 42 (calc (solve ([3,6,(-2),3,21], 42)) [3,6,(-2),3,21]) )
test2 = TestCase (assertEqual "" (-52) (calc (solve ([10,6,12,15,3,13],(-52))) [10,6,12,15,3,13]))
test3 = TestCase (assertEqual "" 1 (calc (solve ([3,2], 1)) [3,2]))
test4 = TestCase (assertEqual "" [] (solve ([42],42)))
test5 = TestCase (assertEqual "" [] (solve ([42],21)))
test6 = TestCase (assertEqual "" [] (solve ([3,2],42)))


tests = TestList [TestLabel "Test 1" test1, TestLabel "Test 2" test2,
                  TestLabel "Test 3" test3, TestLabel "Test 4" test4,
                  TestLabel "Test 5" test5, TestLabel "Test 6" test6]

calc :: [Operators] -> [Integer] -> Integer
calc ops (n:ns) = calc' ops ns n
  where calc' [] [] acc = acc
        calc' (o:ops) (n:ns) acc = case o of
          Plus  -> calc' ops ns (acc + n)
          Minus -> calc' ops ns (acc - n)
          Times -> calc' ops ns (acc * n)
          Div   -> calc' ops ns (acc `div` n)

main = runTestTT tests