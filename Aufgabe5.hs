module Aufgabe5 where

import Debug.Trace

data Tree   = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label  = Integer

tmap :: (Label -> Label) -> Tree -> Tree
tmap _ Null = Null
tmap f (Tree l t1 t2) = Tree (f l) (tmap f t1) (tmap f t2)

tzw :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzw f (Tree l1 t1 t2) (Tree l2 u1 u2) = Tree (f l1 l2) (tzw f t1 u1) (tzw f t2 u2)
tzw _ _ _ = Null

tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label
tfold f zero Null = zero
tfold f zero (Tree l t1 t2) = f l (tfold f zero t1) (tfold f zero t2)



type ErdosNumber = Integer
data Scientist = Sc Initial SurName
type Initial = Char
type SurName = String
type Author = Scientist
newtype Database = Db [([Author],PaperTitle)]
type PaperTitle = String

instance Eq Scientist where
  (Sc i1 s1) == (Sc i2 s2) = i1 == i2 && s1 == s2

instance Show Scientist where
  show (Sc i s) = show (i : s)


db = Db [([Sc 'M' "Smith",Sc 'G' "Martin",Sc 'P' "Erdos"],"Newtonian Forms of Prime Factors"),
         ([Sc 'P' "Erdos",Sc 'W' "Reisig"],"Stuttering in Petri Nets") ,
         ([Sc 'M' "Smith",Sc 'X' "Chen"],"First Order Derivates in Structured Programming"),
         ([Sc 'T' "Jablonski",Sc 'Z' "Hsueh"],"Selfstabilizing Data Structures"),
         ([Sc 'X' "Chen",Sc 'L' "Li"],"Prime Numbers and Beyond")]

erdosNum :: Database -> Scientist -> ErdosNumber
erdosNum db sc = case (maybeErdosNum db sc) of
  Nothing -> -1
  Just n -> (-n)
  where
    maybeErdosNum _ (Sc 'P' "Erdos") = Just 0
    maybeErdosNum (Db db) sc = fmap (subtract 1) maxCoAuthsErdosNum
      where
        maxCoAuthsErdosNum = (maximum $ Nothing : map (maybeErdosNum dbWithoutAuthor) coAuthors)
        coAuthors = concatMap (sc `extractCoAuths`) authoredPapers
        author `extractCoAuths`  (authors, _) = filter (/= author) authors
        authoredPapers = filter (sc `authorOf`) db
        sc `authorOf` (authors, _) = sc `elem` authors
        paper `removeAuth` auth = (auth `extractCoAuths` paper, "")
        dbWithoutAuthor = Db (map (`removeAuth` sc) db)