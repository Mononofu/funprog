{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad (liftM3, forM)
import Data.Maybe

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

getIternaryTime (Route (_, time)) = time
getIternaryTime _ = 0

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
