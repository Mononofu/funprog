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
               (Road "Canada" "US" 5),
               (Air "Austria" "UK" 3),
               (Road "Hungary" "Austria" 2),
               (Air "Austria" "Bulgaria" 4),
               (Road "Hungary" "Romania" 10),
               (Road "Romania" "Bulgaria" 8)
  ]

getIternaryTime (Route (_, time)) = time
getIternaryTime _ = 0

test10 = TestCase (assertEqual "" True (isRoute connections "Austria" "Spain"))
test11 = TestCase (assertEqual "" True (isRoute connections "Austria" "UK"))
test12 = TestCase (assertEqual "" False (isRoute connections "US" "UK"))
test13 = TestCase (assertEqual "" True (isRoute connections "Spain" "Bulgaria"))

test20 = TestCase (assertEqual "" [] (yieldRoute connections "US" "UK"))

test30 = TestCase (assertEqual "" 5 (getIternaryTime (yieldFastestRoute connections "Austria" "Spain")))

test40 = TestCase (assertEqual "" True (isFeelGoodRoute connections "Austria" "Bulgaria" 0))
test41 = TestCase (assertEqual "" False (isFeelGoodRoute connections "Austria" "UK" 0))

test_off_addRev1 = TestCase (assertEqual ""
  ( Success (0, 11, "11"), Failure)
  ( addRev 11 100, addRev (-196)(-400)))

test_off_addRev2 = TestCase (assertEqual ""
  [196]
  [i|i<-[1..196],Failure<-[addRev(-i)(-24)]])

test_off_addRev3 = TestCase (assertEqual ""
  [1,2,3,4,5,6,7,8,9,11,22,33,44,55,66,77,88,99]
  [n|Success (_,n,_)<-[addRev(-i)0|i<-[1..100]]])

test_off_addRev4 = TestCase (assertEqual ""
  90
  (length[n|Success (_,n,_)<-[addRev(-i)0|i<-[100..999]]]))

test_off_isRoute =
  let m=7; r=[1..m]
      g=[Road(show i)(show j)1|i<-r,j<-[i+2],j<=m]
  in TestCase (assertEqual ""
  []
  [(i,j)|i<-r,j<-r,i/=j,isRoute g(show i)(show j)/=((i+1)`mod`2/=j`mod`2)])

test_off_yieldRoute =
  let m=7; r=[1..m]
      g=[Road(show i)(show j)1|i<-r,j<-[i+2],j<=m]
  in TestCase (assertEqual ""
  []
  [(i,j)|i<-r,j<-r,i/=j,isRoute g(show i)(show j)/=(yieldRoute g(show i)(show j)/=[])])

test_off_yieldFastestRoute =
  let m=7; r=[1..m]
      g=[Road(show i)(show j)1|i<-r,j<-[i+2],j<=m]
  in TestCase (assertEqual ""
  []
  [(i,j)|i<-r,j<-r,i/=j,isRoute g(show i)(show j)/=(yieldFastestRoute g(show i)(show j)/=NoRoute)])

test_off_isFeelGoodRoute =
  let m=7; r=[1..m]
      g=[Road(show i)(show j)1|i<-r,j<-[i+2],j<=m]
  in TestCase (assertEqual ""
  []
  [(i,j)|i<-r,j<-r,i/=j,isRoute g(show i)(show j)/=isFeelGoodRoute g(show i)(show j)m])

test_off_yieldFeelGoodRoute =
  let m=7; r=[1..m]
      g=[Road(show i)(show j)1|i<-r,j<-[i+2],j<=m]
  in TestCase (assertEqual ""
  []
  [(i,j)|i<-r,j<-r,i/=j,isRoute g(show i)(show j)/=(yieldFeelGoodRoute g(show i)(show j)m/=NoRoute)])

tests = TestList [TestLabel "Test 1" test1, TestLabel "Test 2" test2,
                  TestLabel "Test 3" test3, TestLabel "Test 4" test4,
                  TestLabel "Test 5" test5, TestLabel "Test 6" test6,
                  TestLabel "Test 7" test7,
                  TestLabel "isRoute" test10, TestLabel "isRoute" test11,
                  TestLabel "isRoute" test12, TestLabel "isRoute" test13,
                  TestLabel "yieldRoute" test20,
                  TestLabel "yieldFastestRoute" test30,
                  TestLabel "isFeelGoodRoute" test40, TestLabel "isFeelGoodRoute" test41,
                  TestLabel "test_off_addRev1" test_off_addRev1,
                  TestLabel "test_off_addRev2" test_off_addRev2,
                  TestLabel "test_off_addRev3" test_off_addRev3,
                  TestLabel "test_off_addRev4" test_off_addRev4,
                  TestLabel "test_off_isRoute" test_off_isRoute,
                  TestLabel "test_off_yieldRoute" test_off_yieldRoute,
                  TestLabel "test_off_yieldFastestRoute" test_off_yieldFastestRoute,
                  TestLabel "test_off_isFeelGoodRoute" test_off_isFeelGoodRoute,
                  TestLabel "test_off_yieldFeelGoodRoute" test_off_yieldFeelGoodRoute]


main = do
  runTestTT tests
