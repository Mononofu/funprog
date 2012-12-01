{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad (liftM3, forM)

import Aufgabe7

test1 = TestCase (assertEqual "" 42 (calc (solve ([3,6,(-2),3,21], 42)) [3,6,(-2),3,21]) )
test2 = TestCase (assertEqual "" (-52) (calc (solve ([10,6,12,15,3,13],(-52))) [10,6,12,15,3,13]))
test3 = TestCase (assertEqual "" 1 (calc (solve ([3,2], 1)) [3,2]))
test4 = TestCase (assertEqual "" [] (solve ([42],42)))
test5 = TestCase (assertEqual "" [] (solve ([42],21)))
test6 = TestCase (assertEqual "" [] (solve ([3,2],42)))


db_with_dup = [
    ("Matrix", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 1999, ScienceFiction, 10)
  , ("Matrix", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 1999, ScienceFiction, 10)
  , ("Matrix", "Andy Wachowski", ["Laurence Fishburne", "Keanu Reeves", "Carrie-Anne Moss"], 1999, ScienceFiction, 10)
  , ("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10)
  ]

db = [
    ("Matrix", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 1999, ScienceFiction, 10),
    ("The Matrix Reloaded", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 2003, ScienceFiction, 10),
    ("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10),
    ("Gran Torino", "Clint Eastwood", ["Clint Eastwood", "Bee Vang"], 2008, Thriller, 12)
  ]


test10 = TestCase (assertEqual "duplicates not removed" (length db_with_dup) (2 + length (rm_dup db_with_dup)))
test11 = TestCase (assertEqual "" (get_tad db 1982) [("Blade Runner", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982)])
test_rtd = TestCase (assertEqual "" (get_rtd db) [("Andy Wachowski", "Matrix", 1999), 
                                                  ("Andy Wachowski", "The Matrix Reloaded", 2003),
                                                  ("Ridley Scott", "Blade Runner", 1982),
                                                  ("Clint Eastwood", "Gran Torino", 2008)])
test_rtg1 = TestCase (assertEqual "" (get_rtg db) [("Clint Eastwood", "Gran Torino", Thriller)])
test_rtg2 = TestCase (assertEqual "" (get_rtg []) [])

test_atr1 = TestCase (assertEqual "" (get_atr db "Keanu Reeves") [("Keanu Reeves", "Matrix", 1999),
                                                                  ("Keanu Reeves", "The Matrix Reloaded", 2003)])
test_atr2 = TestCase (assertEqual "" (get_atr [] "Keanu Reeves") [])
test_atr3 = TestCase (assertEqual "" (get_atr db "Quentin Tarantino") [])

test_dbgri1 = TestCase (assertEqual "" (upd_dbgri db ScienceFiction "Andy Wachowski" 10) 
            [("Matrix", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 1999, ScienceFiction, 20),
            ("The Matrix Reloaded", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 2003, ScienceFiction, 20),
            ("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10),
            ("Gran Torino", "Clint Eastwood", ["Clint Eastwood", "Bee Vang"], 2008, Thriller, 12)])
test_dbgri2 = TestCase (assertEqual "" (upd_dbgri db ScienceFiction "Andy Wachowski" (-10)) 
            [("Matrix", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 1999, ScienceFiction, 1),
            ("The Matrix Reloaded", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 2003, ScienceFiction, 1),
            ("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10),
            ("Gran Torino", "Clint Eastwood", ["Clint Eastwood", "Bee Vang"], 2008, Thriller, 12)])

test_upd_dbad1 = TestCase (assertEqual "" (upd_dbad db "Keanu Reeves" 1990) 
            [("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10),
            ("Gran Torino", "Clint Eastwood", ["Clint Eastwood", "Bee Vang"], 2008, Thriller, 12)])
test_upd_dbad2 = TestCase (assertEqual "" (upd_dbad db "Keanu Reeves" 2000) 
            [("Matrix", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 1999, ScienceFiction, 10),
            ("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10),
            ("Gran Torino", "Clint Eastwood", ["Clint Eastwood", "Bee Vang"], 2008, Thriller, 12)])


test_dbda1 = TestCase (assertEqual "" (get_dbda db 2000 "Keanu Reeves") [("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10)])
test_dbda2 = TestCase (assertEqual "" (get_dbda db 2009 "Keanu Reeves") 
            [("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10),
            ("Gran Torino", "Clint Eastwood", ["Clint Eastwood", "Bee Vang"], 2008, Thriller, 12)])
            

test_sort_dbj = TestCase (assertEqual "" (sort_dbj db) 
            [("Gran Torino", "Clint Eastwood", ["Clint Eastwood", "Bee Vang"], 2008, Thriller, 12),
            ("The Matrix Reloaded", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 2003, ScienceFiction, 10),
            ("Matrix", "Andy Wachowski", ["Keanu Reeves", "Laurence Fishburne", "Carrie-Anne Moss"], 1999, ScienceFiction, 10),
            ("Blade Runner", "Ridley Scott", ["Harrison Ford", "Rutger Hauer", "Sean Young "], 1982, ScienceFiction, 10)])



db_genre = [
    ("Comedy M1", "Res A", [], 1999, Comedy, 10),
    ("Fantasy M1", "Res X", [], 1999, Fantasy, 10),
    ("SF M1", "Res X", [], 1999, ScienceFiction, 10),
    ("Fantasy M2", "Res A", [], 1999, Fantasy, 10),
    ("SF M2", "Res C", [], 1999, ScienceFiction, 10),
    ("SF M3", "Res A", [], 1999, ScienceFiction, 10),
    ("Thriller M1", "Res X", [], 1999, Thriller, 10)]
db_sorted_genre = [
    ("Thriller M1", "Res X", [], 1999, Thriller, 10),
    ("Fantasy M2", "Res A", [], 1999, Fantasy, 10),
    ("Fantasy M1", "Res X", [], 1999, Fantasy, 10),
    ("SF M3", "Res A", [], 1999, ScienceFiction, 10),
    ("SF M2", "Res C", [], 1999, ScienceFiction, 10),
    ("SF M1", "Res X", [], 1999, ScienceFiction, 10),
    ("Comedy M1", "Res A", [], 1999, Comedy, 10)]
test_sort_dbgr = TestCase (assertEqual "" (sort_dbgr db_genre) db_sorted_genre)

tests = TestList [TestLabel "Test 1" test1, TestLabel "Test 2" test2,
                  TestLabel "Test 3" test3, TestLabel "Test 4" test4,
                  TestLabel "Test 5" test5, TestLabel "Test 6" test6,
                  TestLabel "rm_dup" test10, TestLabel "get_tad" test11,
                  TestLabel "get_rtd" test_rtd, 
                  TestLabel "get_rtg" test_rtg1, TestLabel "get_rtg empty list" test_rtg2,
                  TestLabel "get_atr" test_atr1, TestLabel "get_atr" test_atr2, TestLabel "get_atr" test_atr3,
                  TestLabel "upd_dbgri" test_dbgri1, TestLabel "upd_dbgri" test_dbgri2,
                  TestLabel "upd_dbad" test_upd_dbad1, TestLabel "upd_dbad" test_upd_dbad2,
                  TestLabel "get_dbda" test_dbda1, TestLabel "get_dbda" test_dbda2,
                  TestLabel "sort_dbj" test_sort_dbj, TestLabel "sort_dbgr" test_sort_dbgr]

calc :: [Operators] -> [Integer] -> Integer
calc ops (n:ns) = calc' ops ns n
  where calc' [] [] acc = acc
        calc' (o:ops) (n:ns) acc = case o of
          Plus  -> calc' ops ns (acc + n)
          Minus -> calc' ops ns (acc - n)
          Times -> calc' ops ns (acc * n)
          Div   -> calc' ops ns (acc `div` n)



instance Arbitrary Operators where
  arbitrary = elements [Plus, Times, Minus, Div]


newtype Values = Values ([Integer], [Operators])
  deriving Show

instance Arbitrary Values where
  arbitrary = do
    n <- choose(1, 10) :: Gen Int
    ns <- forM [1..n] $ \_ -> do
      i <- choose(1, 40) :: Gen Integer
      return i
    ops <- forM [1..(n-1)] $ \_ -> do
      op <- arbitrary
      return op
    return (Values (ns, ops))

prop_solve (Values (ns, ops)) = calc (solve (ns, n)) ns == n
  where n = calc ops ns

main = do
  $(quickCheckAll)
  runTestTT tests
