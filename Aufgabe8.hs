module Aufgabe8 where

import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Data.List (sortBy)

type InitialValue     = Integer
type NumberOfRounds   = Integer
type MaxRounds        = Integer
type FinalValue       = Integer
type TextRep          = String
data Solution         = Failure | Success (NumberOfRounds,FinalValue,TextRep)
                          deriving (Eq, Show)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y


isPalindrome :: String -> Bool
isPalindrome xs = xs == (reverse xs)

addRev :: InitialValue -> MaxRounds -> Solution
addRev n r = addRev' (abs n) (abs r) 0

addRev' n 0 roundsUsed
  | isPalindrome (show n) = Success (roundsUsed, n, show n)
  | otherwise             = Failure
addRev' n roundsLeft roundsUsed
  | isPalindrome (show n) = Success (roundsUsed, n, (show n))
  | otherwise             = addRev' m (roundsLeft-1) (roundsUsed+1)
  where n' = read $ reverse $ show n
        m = n + n'


type Country      = String
type Countries    = [Country]
type TravelTime   = Integer -- Travel time in minutes
data Connection   = Air Country Country TravelTime
                  | Sea Country Country TravelTime
                  | Rail Country Country TravelTime
                  | Road Country Country TravelTime deriving (Eq,Ord,Show)
type Connections  = [Connection]
data Itinerary    = NoRoute | Route (Connections,TravelTime) deriving (Eq,Ord,Show)

type Node           = Country
-- data ConnectionType = AirC | SeaC | RailC | RoadC deriving (Eq, Ord, Show)
type ConnectionInfo = Connection
type Edges          = Map.Map Node [ConnectionInfo]
type Graph          = Map.Map Node Edges

parseConnection c@(Air from to time)  = (from, to, c)
parseConnection c@(Sea from to time)  = (from, to, c)
parseConnection c@(Rail from to time) = (from, to, c)
parseConnection c@(Road from to time) = (from, to, c)

getTime (Air _ _ time) = abs time
getTime (Sea _ _ time) = abs time
getTime (Rail _ _ time) = abs time
getTime (Road _ _ time) = abs time

makeGraph :: Connections -> Graph
makeGraph []      = Map.empty
makeGraph (c:cs)  = Map.alter (addNode to) from $ Map.alter (addNode from) to oldGraph
  where
    oldGraph = makeGraph cs
    addNode :: Node -> Maybe Edges -> Maybe Edges
    addNode node Nothing      = Just $ Map.fromList [(node, [info])]
    addNode node (Just edges) = Just $ Map.alter (addInfo) node edges
    addInfo Nothing       = Just $ [info]
    addInfo (Just infos)  = Just $ info : infos
    (from, to, info) = parseConnection c

-- Gibt es eine Reiseroute von Land A nach Land B?
isRoute :: Connections -> Country -> Country -> Bool
isRoute cs start end = search [] start
  where
    graph = (makeGraph cs)
    search nodesVisited cur  = case Map.lookup cur graph of
      Nothing     -> False
      Just edges
        | end `Map.member` edges  -> True
        | otherwise                    ->
          let freshNodes = filter (`notElem` nodesVisited) (Map.keys edges)
          in or $ map (\n -> search (n:nodesVisited) n) freshNodes


-- Liefere eine Reiseroute von Land A nach Land B, falls es eine solche gibt,
-- ansonsten die leere Liste. Gibt es mehrere solche Reiserouten, so ist es egal,
-- welche dieser Routen als Ergebnis geliefert wird:
yieldRoute :: Connections -> Country -> Country -> Connections
yieldRoute cs start end = fromMaybe [] $ search [] start
  where
    graph = (makeGraph cs)
    search nodesVisited cur  = do
      edges <- Map.lookup cur graph
      if end `Map.member` edges  then do
        connInfo <- Map.lookup end edges
        return [(head connInfo)]
      else
        let freshNodes = filter (`notElem` nodesVisited) (Map.keys edges)
        in maybeHead $ catMaybes $ map (searchNode nodesVisited cur) freshNodes
    searchNode nodesVisited cur n = do
        path      <- search (n:nodesVisited) n
        edges     <- Map.lookup cur graph
        connInfo  <- Map.lookup n edges
        return ((head connInfo) : path)

maybeHead [] = Nothing
maybeHead xs = Just $ head xs

-- Liefere die schnellste Reiseroute von Land A nach Land B, falls es eine solche
-- gibt, zusammen mit der Gesamtreisedauer; ansonsten den Wert NoRoute. Gibt es
-- mehrere solcher Reiserouten, so ist es egal, welche dieser Routen als Ergebnis
-- geliefert wird:
yieldFastestRoute :: Connections -> Country -> Country -> Itinerary
yieldFastestRoute cs start end = fromMaybe NoRoute $ do
  paths <- yieldAllRoutes cs start end
  let sortedPaths = sortBy (compare `on` snd) $ map pairWithTravelTime paths
  fastestPath <- maybeHead sortedPaths
  return (Route fastestPath)
  where
    pairWithTravelTime path = (path, foldl1 (+) $ map getTime path)

yieldAllRoutes :: Connections -> Country -> Country -> Maybe [[ConnectionInfo]]
yieldAllRoutes cs start end = search [] start
  where
    graph = (makeGraph cs)

    search :: [Node] -> Node -> Maybe [[ConnectionInfo]]
    search nodesVisited cur  = do
      edges <- Map.lookup cur graph
      let freshNodes = filter (`notElem` nodesVisited) (Map.keys edges)
      return $ concat $ catMaybes $ map (searchNode nodesVisited cur) freshNodes

    searchNode :: [Node] -> Node -> Node -> Maybe [[ConnectionInfo]]
    searchNode nodesVisited cur n = do
        paths     <- search (n:nodesVisited) n
        edges     <- Map.lookup cur graph
        connInfo  <- Map.lookup n edges
        if n == end then return [ [(head connInfo)] ]
        else return $ map ((head connInfo):) paths

-- Gibt es eine Reiseroute von Land A nach Land B, auf der die Summe aus Flug-
-- und Seezeiten hochstens max Minuten betragt?
isFeelGoodRoute :: Connections -> Country -> Country -> TravelTime -> Bool
isFeelGoodRoute cs start end maxAirSeeTime =
  case yieldFeelGoodRoute cs start end maxAirSeeTime of
    NoRoute -> False
    Route _ -> True

-- Liefere eine Reiseroute von Land A nach Land B, auf der die Summe aus Flug-
-- und Seezeiten hochstens max Minuten betragt, falls es eine solche gibt,
-- zusammen mit dieser Reisezeit; ansonsten den Wert NoRoute. Gibt es mehrere
-- solcher Reiserouten, so ist es egal, welche dieser Routen als Ergebnis
-- geliefert wird:
yieldFeelGoodRoute :: Connections -> Country -> Country -> TravelTime -> Itinerary
yieldFeelGoodRoute cs start end maxAirSeeTime = fromMaybe NoRoute $ do
  paths <- yieldAllRoutes cs start end
  let feelGoodPaths = filter (\p -> airSeaTime p <= maxAirSeeTime) paths
  feelGoodRoute <- maybeHead feelGoodPaths
  return $ Route (feelGoodRoute, airSeaTime feelGoodRoute)
  where
    airSeaTime path = foldl1 (+) $ map getAirSeaTime path
    getAirSeaTime (Air _ _ time) = time
    getAirSeaTime (Sea _ _ time) = time
    getAirSeaTime _              = 0
