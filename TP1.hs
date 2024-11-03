import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- Auxiliary functions
-- nub function created in tp2
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = x : mynub (filter (/= x) xs)

-- elem function created in tp2
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem x (y:ys)
    | x == y = True
    | otherwise = myelem x ys

-- Auxiliary function to find the minimum in a list
findMinimum :: (a -> a -> Ordering) -> [a] -> a
findMinimum _ [x] = x
findMinimum cmp (x:y:xs)
    | cmp x y == LT = findMinimum cmp (x:xs)
    | otherwise = findMinimum cmp (y:xs)

-- Auxiliary function to get the value from a Maybe
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nothing"

--cities function - 1
cities :: RoadMap -> [City]
cities r = mynub ([city1 | (city1, _ , _ ) <- r] ++ [city2 | (_ , city2, _) <- r])

--areAdjacent function - 2
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] c1 c2 = False
areAdjacent ((x,y,_):xs) c1 c2 
    | (c1 == x && c2 == y) || (c2 == y && c1 == x) = True
    | otherwise = areAdjacent xs c1 c2


--distance function - 3
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] c1 c2 = Nothing
distance ((x,y,z):xs) c1 c2
    | (c1 == x && c2 == y) || (c1 == y && c2 == x) = Just(z)
    |otherwise = distance xs c1 c2


--adjacent function - 4
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent r c = [(y, dist) | (x, y, dist) <- r, c == x] ++ [(x, dist) | (x, y, dist) <- r, c == y]


--pathDistance function - 5
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0  -- Empty path has a distance of 0
pathDistance _ [_] = Just 0 -- Path with only one city has a distance of 0
pathDistance r (x:y:xs) = case distance r x y of
    Nothing -> Nothing -- If no road is found between consecutive cities
    Just dist -> case pathDistance r (y:xs) of
        Nothing -> Nothing -- If a subsequent segment is not connected
        Just rest -> Just (dist + rest)

--rome function - 6
maximuml :: RoadMap -> Int
maximuml r = maximum [length(adjacent r c1) | (c1,c2,dist) <- r]

rome :: RoadMap -> [City]
rome r = mynub [c1 | (c1,c2,dist) <- r, length(adjacent r c1) == maximuml r] 


--isStronglyConnected function - 7
dfs :: RoadMap -> City -> [City] -> [City]
dfs r c visited 
 | myelem c visited = visited
 | otherwise = foldl (\acc (adjCity, _) -> dfs r adjCity acc) (c : visited) (adjacent r c)

-- função para chegar a todas as reachable cities a partir de uma determinada city
reachableCities :: RoadMap -> City -> [City]
reachableCities r c = dfs r c []

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected r
    | null (cities r) = True  -- If there are no cities, it's trivially strongly connected
    | otherwise = all (\c -> length (reachableCities r c) == length (cities r)) (cities r)


-- shortestPath function - 8
-- Function to update distances for adjacent cities of the current city
updateDistances :: RoadMap -> City -> [(City, Distance)] -> [(City, Distance)] -> [(City, Distance)]
updateDistances r current distances visited = foldr update distances (adjacent r current)
  where
    currentDist = lookup current distances
    update (neighbor, dist) dists =
      case currentDist of
        Nothing -> dists
        Just cd -> case lookup neighbor dists of
          Nothing -> (neighbor, cd + dist) : dists
          Just nd -> 
            if cd + dist < nd 
            then (neighbor, cd + dist) : filter ((/= neighbor) . fst) dists 
            else dists

-- Helper function to find the city with the minimum distance that hasn't been visited
findMinDistance :: [(City, Distance)] -> [(City, Distance)] -> Maybe City
findMinDistance distances visited = case filter (\(c, _) -> not (myelem c (map fst visited))) distances of
  [] -> Nothing
  unvisited -> Just (fst (findMinimum (\(_, d1) (_, d2) -> compare d1 d2) unvisited))

-- Dijkstra's algorithm to find the shortest path distance between two cities
dijkstra :: RoadMap -> City -> City -> Maybe (Path, Distance)
dijkstra r start goal = dijkstraAux [(start, 0)] [] [start]
  where
    dijkstraAux distances visited path
      | null distances = Nothing
      | current == goal = Just (reverse path, fromJust (lookup current distances))
      | otherwise = dijkstraAux newDistances ((current, fromJust (lookup current distances)) : visited) (current : path)
      where
        current = fromJust (findMinDistance distances visited)
        newDistances = updateDistances r current distances visited

-- Helper function to gather all paths with the exact shortest distance
shortestPathAux :: RoadMap -> City -> City -> Distance -> Path -> [Path]
shortestPathAux r current goal dist path
  | current == goal && dist == 0 = [reverse path]
  | otherwise = concatMap (\(nextCity, d) -> 
        if d <= dist 
        then shortestPathAux r nextCity goal (dist - d) (nextCity : path)
        else [])
      validNextCities
  where
    validNextCities = filter (\(c, _) -> not (myelem c path)) (adjacent r current)


-- Modified function to get all shortest paths
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath r c1 c2
  | c1 == c2 = [[c1]]
  | otherwise = case dijkstra r c1 c2 of
      Nothing -> []
      Just (_, dist) -> shortestPathAux r c1 c2 dist [c1]


-- travelSales function - 9
-- Helper function to find the closest city not yet visited - Nearest Neighbour heuristic   
closestCity :: RoadMap -> City -> [City] -> Maybe (City, Distance)
closestCity r c visited = 
    let unvisited = filter (\(city, _) -> not (myelem city visited)) (adjacent r c) --verify if the city is visited
    in case unvisited of
    [] -> Nothing   -- No unvisited city
    _  -> Just (findMinimum (\(_, d1) (_, d2) -> compare d1 d2) unvisited)  -- Returns the closest city


-- Greedy TSP approximation
travelSalesAux :: RoadMap -> City -> [City] -> Path -> Distance -> [(Path, Distance)]
travelSalesAux _ _ [] path totalDist = [(reverse path, totalDist)]  -- Returns the path when all cities have been visited
travelSalesAux r currentCity unvisitedCities path totalDist =
    let nextCities = filter (\(city, _) -> not (myelem city path)) (adjacent r currentCity)
    in if null nextCities then []  -- No path found
       else concatMap (\(nextCity, dist) -> travelSalesAux r nextCity (filter (/= nextCity) unvisitedCities) (nextCity : path) (totalDist + dist)) nextCities

-- Main TSP function that starts from an initial city
travelSalesFromCity :: RoadMap -> City -> Path
travelSalesFromCity r startCity =
    let allPaths = travelSalesAux r startCity (filter (/= startCity) (cities r)) [startCity] 0
    in case allPaths of
        [] -> []  -- No path found 
        _  -> let (path, totalDist) = findMinimum (\(_, d1) (_, d2) -> compare d1 d2) allPaths
              in case distance r (last path) startCity of  -- Tries to find the path back to the start
                    Nothing -> []  -- If no return path, returns empty path
                    Just dist -> path ++ [startCity]  -- Completes the return cycle

-- Main TSP function that tries starting from different cities
travelSales :: RoadMap -> Path
travelSales r =
    let allCities = cities r
        allPaths = map (travelSalesFromCity r) allCities
        validPaths = filter (not . null) allPaths
    in if null validPaths then [] else head validPaths

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]


