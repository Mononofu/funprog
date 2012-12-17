{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad (liftM3, forM)
import Data.Maybe
import Data.List (sort)

import Aufgabe9


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

completeConnections = [
               (Air "Austria" "Germany" 2),
               (Road "Austria" "Germany" 10),
               (Air "Germany" "Spain" 3),
               (Sea "Spain" "UK" 15),
               (Air "US" "UK" 10),
               (Road "Canada" "US" 5),
               (Air "Canada" "US" 2)
  ]

roundConnections = [
  (Air "China" "Japan" 3),
  (Air "China" "India" 10),
  (Air "India" "Australia" 8),
  (Air "Australia" "Japan" 5)]

getIternaryTime (Route (_, time)) = time
getIternaryTime _ = 0

test_yieldUnreachable1 = TestCase (assertEqual ""
  (sort ["Canada", "US"])
  (sort (yieldUnreachable connections "Austria")))

test_yieldUnreachable2 = TestCase (assertEqual ""
  (sort ["Austria","Germany","Spain","UK","Hungary","Bulgaria","Romania"])
  (sort (yieldUnreachable connections "US")))

-- assuming that the start country should be part of the reachable list too
test_yieldGroundReachable1 = TestCase (assertEqual ""
  (sort ["Austria","Germany","Hungary","Romania","Bulgaria"])
  (sort (yieldGroundReachable connections "Austria")))

test_yieldGroundReachable2 = TestCase (assertEqual ""
  (sort ["Canada","US"])
  (sort (yieldGroundReachable connections "US")))


test_isRoundTrip1 = TestCase (assertEqual ""
  True
  (isRoundTrip [(Air "Japan" "China" 5)] "Japan"))

test_isRoundTrip2 = TestCase (assertEqual ""
  True
  (isRoundTrip roundConnections "Japan"))

test_isRoundTrip3 = TestCase (assertEqual ""
  False
  (isRoundTrip completeConnections "Austria"))

test_isRoundTrip4 = TestCase (assertEqual ""
  False
  (isRoundTrip connections "Austria"))


-- the first 22 lucky numbers, according to wikipedia
lucky_numbers_22 = [3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79, 87, 93, 99]

test_luckynums_take22 = TestCase (assertEqual ""
  lucky_numbers_22
  (take 22 luckyNumbers))

test_luckynums_isLuckyNumber = TestCase (assertEqual ""
  lucky_numbers_22
  [n | n <- [1..100], isLuckyNumber n])

test_luckynums_yieldLuckyNumbers = TestCase (assertEqual ""
  lucky_numbers_22
  (yieldLuckyNumbers 1 100))

test_luckynums_isTwinLuckyNumber = TestCase (assertEqual ""
  [isTwinLuckyNumber n | n <- [1..98]]
  ([if n `elem` lucky_numbers_22 then
          (if (n+2) `elem` lucky_numbers_22 || (n-2) `elem` lucky_numbers_22 then True
           else False)
        else False | n <- [1..98]]))

tests = TestList [
  TestLabel "test_yieldUnreachable1" test_yieldUnreachable1,
  TestLabel "test_yieldUnreachable2" test_yieldUnreachable2,
  TestLabel "test_yieldGroundReachable1" test_yieldGroundReachable1,
  TestLabel "test_yieldGroundReachable2" test_yieldGroundReachable2,
  TestLabel "test_isRoundTrip1 singleton list" test_isRoundTrip1,
  TestLabel "test_isRoundTrip2" test_isRoundTrip2,
  TestLabel "test_isRoundTrip3" test_isRoundTrip3,
  TestLabel "test_isRoundTrip4" test_isRoundTrip4,
  TestLabel "luckyNumbers take 22" test_luckynums_take22,
  TestLabel "isLuckyNumbers" test_luckynums_isLuckyNumber,
  TestLabel "test_luckynums_yieldLuckyNumbers" test_luckynums_yieldLuckyNumbers,
  TestLabel "test_luckynums_isTwinLuckyNumber" test_luckynums_isTwinLuckyNumber]

prop_yieldLuckyNumbers m n
  | m > n     = nums == []
  | otherwise = case nums of
    []    -> True
    nums  -> head nums >= m && last nums <= n
  where nums = yieldLuckyNumbers m n

main = do
  $(quickCheckAll)
  runTestTT tests
