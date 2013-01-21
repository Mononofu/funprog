import Data.List
import qualified Data.Map as Map

ggt :: Integer -> Integer -> Integer
ggt 0 n = n
ggt n 0 = n
ggt n m 
  | n > m     = ggt (n-m) m
  | otherwise = ggt (m-n) n

kgv :: Integer -> Integer -> Integer
kgv 0 _ = 0
kgv _ 0 = 0
kgv n m = i * j `div` (ggt i j)
  where 
    i = abs n
    j = abs m


agv :: Integer -> Integer -> (Integer, Integer) -> [Integer]
agv 0 _ _ = []
agv _ 0 _ = []
agv m n (p, q) = dropWhile (< p) $ takeWhile (<= q) [i * k | i <- [1..]]
  where
    k = kgv (abs m) (abs n)


type PassName 		= String
type FlightNumber	= Integer
type PlaceOfDeparture	= String
type Destination	= String
type Airfare		= Integer
type Database		= [(PassName, FlightNumber, PlaceOfDeparture, Destination, Airfare)]

getName :: (PassName, FlightNumber, PlaceOfDeparture, Destination, Airfare) -> PassName
getName (name, _, _, _, _) = name

getFare :: (PassName, FlightNumber, PlaceOfDeparture, Destination, Airfare) -> Airfare
getFare (_, _, _, _, fare) = fare


flights :: Database -> PassName -> [(FlightNumber,Airfare)]
flights db name = sortBy comparePrice $ map condense $ filter isName db
  where 
    isName (passname, _, _, _, _) = name == passname
    condense (_, flightnumber, _, _, airfare) = (flightnumber, airfare)
    comparePrice (fnum1, _) (fnum2, _) = compare fnum1 fnum2


pass2Dest :: Database -> Destination -> [PassName]
pass2Dest db dest = sort $ map getName $ filter isDest db
  where
    isDest (_, _, _, destination, _) = dest == destination
    

mostValuedPass :: Database -> PlaceOfDeparture -> Destination -> ([PassName],Airfare)
mostValuedPass db dep dest = (reverse $ pass2Dest (takeWhile (isFare highestFare) flights) dest, highestFare)
  where
    highestFare = getFare $ head flights
    flights = sortBy compareFare $ filter isConnection db
    isConnection (_, _, departure, destination, _) = dep == departure && dest == destination
    isFare p record = p == (getFare record)
    compareFare r1 r2 = compare (getFare r2) (getFare r1)
    


crypttexts = ["vtz ud xnm xugm itr pyy jttk gmv xt otgm xt xnm puk ti xnm fprxq",
  "xnm ceuob lrtzv ita hegfd tsmr xnm ypwq ktj",
  "frtjrpgguvj otvxmdxd prm iev prmvx xnmq"]

known_plaintext = "the quick brown fox jumps over the lazy dog"

calcMappings :: String -> [String] -> [Map.Map Char Char]
calcMappings plain crypts = map calcMapping $ filter possibleCrypttext crypts
  where 
    calcMapping crypt = Map.fromList $ zip crypt plain 
    possibleCrypttext crypt = (length plain == length crypt) && (all possibleCryptchar $ zip plain crypt) 
    possibleCryptchar (plain, crypt)
      | plain /= crypt && (plain == ' ' || crypt == ' ') = False
      | otherwise 					 = True

decrypt :: Ord k => Map.Map k Char -> [k] -> [Char]
decrypt mapping crypttext = [mapping Map.! c | c <- crypttext]

allDecryptions plain crypts = [[decrypt key crypt | crypt <- crypts] | key <- calcMappings plain crypts]

