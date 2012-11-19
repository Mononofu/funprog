{-# LANGUAGE TemplateHaskell #-}

import Aufgabe5

import Data.List (nub)
import Test.QuickCheck
import Test.QuickCheck.All

import Control.Monad (liftM3, forM)

f_add = \x y z -> x+y+z
f_mul = \x y z -> x*y*z

instance Arbitrary Tree where
  arbitrary     =
    oneof [ return Null
          , return Null
          , liftM3 Tree arbitrary arbitrary arbitrary ]

flatten :: Tree -> [Integer]
flatten Null = []
flatten (Tree label left right) = label : (flatten left) ++ (flatten right)


prop_revertible tree = (tmap (+1) $ tmap (subtract 1) tree) == tree

prop_zip_with1 t1 t2 = tzw (+) t1 t2 == tzw (+) t2 t1
prop_zip_with2 t1 t2 = tzw (*) t1 t2 == tzw (*) t2 t1

prop_fold1 tree = tfold f_add 0 tree == foldr (+) 0 (flatten tree)
prop_fold2 tree = tfold f_mul 1 tree == foldr (*) 1 (flatten tree)

prop_zip_with3 t1 t2 = tfold f_add 0 (tzw (*) t1 (tmap (\_ -> 0) t2)) == 0


instance Arbitrary Scientist where
  arbitrary     = do
    n <- choose(1, 5) :: Gen Int
    case n of
      1 -> return (Sc 'P' "Erdos")
      _ -> do
        initial <- elements (['A'..'Z'] ++ ['a' .. 'z'])
        nameLength <- choose(2, 5) :: Gen Int
        name <- vectorOf nameLength $ elements (['A'..'Z'] ++ ['a' .. 'z'])
        return (Sc initial name)

instance Arbitrary Database where
  arbitrary      = do
    numPapers <- choose(1, 5) :: Gen Int
    papers <- forM [1..numPapers] $ \_ -> do
      numAuthors <- choose(1, 5) :: Gen Int
      authors <- forM [1..numAuthors] $ \_ -> do
        author <- arbitrary
        return author
      return (authors, "")
    return (Db papers)

instance Show Database where
  show (Db db) = show db

prop_verify_erdos database@(Db db) = verify [scientist] $ erdosNum database scientist
  where
    scientist = case head db of
      (auths, _) -> head auths
    verify _ (-1) = True
    verify scientists 0 = (Sc 'P' "Erdos") `elem` scientists
    verify scientists n =
      verify (extractScientists (filter (scientists `authorOf`) db)) (n-1)
    scientists `authorOf` (authors, _) = any (`elem` authors) scientists
    extractScientists db = nub $ concatMap (\(scs, _) -> scs) db

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)