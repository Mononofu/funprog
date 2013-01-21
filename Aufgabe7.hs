module Aufgabe7 where 

import Data.List

type Movie        = (Title, Regisseur, MainActors, ReleaseDate, Genre, SalesPrice)
type Title        = String
type Regisseur    = String
type Actor        = String
type MainActors   = [Actor]
type ReleaseDate  = Int
data Genre        = Thriller | Fantasy | ScienceFiction | Comedy deriving (Eq,Ord,Show)
type SalesPrice   = Int
type Database     = [Movie]

newtype MyMovie = MyMovie (Title, Regisseur, MainActors, ReleaseDate, Genre, SalesPrice)

instance Eq MyMovie where
  (MyMovie (t1, r1, m1, d1, g1, s1)) == (MyMovie (t2, r2, m2, d2, g2, s2)) = 
    t1 == t2 && r1 == r2 && sort m1 == sort m2 && d1 == d2 && g1 == g2 && s1 == s2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a, b, c, d, e, f) where
  (a1, b1, c1, d1, e1, f1) == (a2, b2, c2, d2, e2, f2) = and [a1 == a2, b1 == b2, 
    c1 == c2, d1 == d2, e1 == e2, f1 == f2]

rm_dup :: Database -> Database
rm_dup movies = map (\(MyMovie m) -> m) $ nub (map (\m -> MyMovie m) movies)

get_rtd :: Database -> [(Regisseur,Title,ReleaseDate)]
get_rtd = map m2rtd
  where m2rtd (title, regisseur, _, date, _, _) = (regisseur, title, date)

get_rtg :: Database -> [(Regisseur,Title,Genre)]
get_rtg = map m2rtg . filter selfActing
  where selfActing (_, regisseur, actors, _, _, _) = regisseur `elem` actors
        m2rtg (title, regisseur, _, _, genre, _) = (regisseur, title, genre)

get_tad :: Database -> ReleaseDate -> [(Title,MainActors,ReleaseDate)]
get_tad db date = map m2tad $ filter inYear db
  where inYear (_, _, _, year, _, _) = year == date
        m2tad (title, _, actors, date, _, _) = (title, actors, date)

get_atr :: Database -> Actor -> [(Actor,Title,ReleaseDate)]
get_atr db actor = map m2atr $ filter withActor db
  where withActor (_, _, actors, _, _, _) = actor `elem` actors
        m2atr (title, _, _, date, _, _) = (actor, title, date)

upd_dbgri :: Database -> Genre -> Regisseur -> Int -> Database
upd_dbgri db genre regisseur change = map selectMovie db
  where selectMovie (t, r, m, d, g, s) = if g == genre && r == regisseur 
        then (t, r, m, d, g, adaptPrice s)
        else (t, r, m, d, g, s)
        adaptPrice s = max 1 (s + change)

upd_dbad :: Database -> Actor -> ReleaseDate -> Database
upd_dbad db actor date = filter keep db
  where keep (_, _, actors, year, _, _) = not (actor `elem` actors && year >= date)

get_dbda :: Database -> ReleaseDate -> Actor -> Database
get_dbda db date actor = filter keep db
  where keep (_, _, actors, year, _, _) = year <= date && (not (actor `elem` actors))

sort_dbj :: Database -> Database
sort_dbj = sortBy yearDesc
 where yearDesc (_, _, _, y1, _, _) (_, _, _, y2, _, _) = compare y2 y1

sort_dbgr :: Database -> Database
sort_dbgr = sortBy genreAsc . sortBy regisseurAsc
  where genreAsc (_, _, _, _, g1, _) (_, _, _, _, g2, _) = compare g1 g2
        regisseurAsc (_, r1, _, _, _, _) (_, r2, _, _, _, _) = compare r1 r2


type ListOfValues = [Integer]
type TargetValue  = Integer
type Game         = (ListOfValues, TargetValue)
data Operators    = Plus | Times | Minus | Div deriving (Eq, Ord, Show)
type Solution     = [Operators]

solve :: Game -> Solution
solve (v:vs, target) = case calc vs v of
  [] -> []
  ops -> init ops
  where calc [] acc = if acc == target then [Plus] else []
        calc (v:vs) acc = 
          case calc vs (acc + v) of 
            [] -> case calc vs (acc * v) of
              [] -> case calc vs (acc - v) of
                [] -> case calc vs (acc `div` v) of
                  [] -> []
                  ops -> Div : ops
                ops -> Minus : ops
              ops -> Times : ops
            ops -> Plus : ops