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


test_luckynums_take5 = TestCase (assertEqual ""
  [3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79, 87, 93, 99]
  (take 22 luckyNumbers))

tests = TestList [
  TestLabel "luckyNumbers take 5" test_luckynums_take5]

prop_yieldLuckyNumbers m n
  | m > n     = nums == []
  | otherwise = case nums of
    []    -> True
    nums  -> head nums >= m && last nums <= n
  where nums = yieldLuckyNumbers m n

main = do
  $(quickCheckAll)
  runTestTT tests
