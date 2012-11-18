{-# LANGUAGE TemplateHaskell #-}

import Aufgabe5

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad (liftM3)

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


-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)