import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--função nub criada na tp2
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = x : mynub (filter (/= x) xs)

--função elem criada na tp2
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem x (y:ys)
    | x==y = True
    | otherwise = myelem x ys


cities :: RoadMap -> [City]
cities r = mynub ([city1 | (city1, _ , _ ) <- r] ++ [city2 | (_ , city2, _) <- r])


areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] c1 c2 = False
areAdjacent ((x,y,_):xs) c1 c2 
    | (c1 == x && c2 == y) || (c2 == y && c1 == x) = True
    | otherwise = areAdjacent xs c1 c2


distance :: RoadMap -> City -> City -> Maybe Distance
distance [] c1 c2 = Nothing
distance ((x,y,z):xs) c1 c2
    | (c1 == x && c2 == y) || (c1 == y && c2 == x) = Just(z)
    |otherwise = distance xs c1 c2


adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent r c = [(y, dist) | (x, y, dist) <- r, c == x] ++ [(x, dist) | (x, y, dist) <- r, c == y]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0  -- Empty path has a distance of 0
pathDistance _ [_] = Just 0 -- Path with only one city has a distance of 0
pathDistance r (x:y:xs) = case distance r x y of
    Nothing -> Nothing -- If no road is found between consecutive cities
    Just dist -> case pathDistance r (y:xs) of
        Nothing -> Nothing -- If a subsequent segment is not connected
        Just rest -> Just (dist + rest)


maximuml :: RoadMap -> Int
maximuml r = maximum [length(adjacent r c1) | (c1,c2,dist) <- r]

rome :: RoadMap -> [City]
rome r = mynub [c1 | (c1,c2,dist) <- r, length(adjacent r c1) == maximuml r] 


--dfs 
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


-- 8. shortestPath :: RoadMap -> City -> City -> [Path], computes all

newPaths :: RoadMap -> City -> Path -> [(City, Distance)]
newPaths r c1 p = [(city, d) | (city, d) <- adjacent r c1, not (myelem city p)]

pathDistances :: RoadMap -> City -> City -> [(Path, Distance)]
pathDistances r c1 c2 = [(path, dist) | path <- shortestPathAux r c1 c2 [c1] 0, Just dist <- [pathDistance r path]]

shortestPathAux :: RoadMap -> City -> City -> Path -> Distance -> [Path]
shortestPathAux r c1 c2 path dist
    | c1 == c2 = [reverse path] 
    | otherwise = concatMap (\(city, d) -> shortestPathAux r city c2 (city:path) (dist + d)) (newPaths r c1 path)

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath r c1 c2
    | c1 == c2 = [[c1]] 
    | otherwise = [path | (path, dist) <- (pathDistances r c1 c2), dist == minimum (map snd (pathDistances r c1 c2))] 


-- 9. travelSales :: RoadMap -> Path, computes a path that visits all cities in the graph and returns to the starting city, with the smallest total distance
-- Helper function to find the minimum element by a given comparison function
findMinimum :: (a -> a -> Ordering) -> [a] -> a
findMinimum _ [x] = x
findMinimum cmp (x:y:xs)
    | cmp x y == LT = findMinimum cmp (x:xs)
    | otherwise = findMinimum cmp (y:xs)


-- Helper function to find the closest city not yet visited - Nearest Neighbour heuristic   
closestCity :: RoadMap -> City -> [City] -> Maybe (City, Distance)
closestCity r c visited = 
    let unvisited = filter (\(city, _) -> not (myelem city visited)) (adjacent r c) --verifica se a cidade já foi visitada 
    in case unvisited of
    [] -> Nothing  -- Nenhuma cidade não visitada
    _  -> Just (findMinimum (\(_, d1) (_, d2) -> compare d1 d2) unvisited)  -- Retorna a cidade mais próxima


-- Greedy TSP approximation
travelSalesAux :: RoadMap -> City -> [City] -> Path -> Distance -> [(Path, Distance)]
travelSalesAux _ _ [] path totalDist = [(reverse path, totalDist)]  -- Retorna o caminho quando todas as cidades foram visitadas
travelSalesAux r currentCity unvisitedCities path totalDist =
    let nextCities = filter (\(city, _) -> not (myelem city path)) (adjacent r currentCity)
    in if null nextCities then []  -- Nenhum caminho encontrado
       else concatMap (\(nextCity, dist) -> travelSalesAux r nextCity (filter (/= nextCity) unvisitedCities) (nextCity : path) (totalDist + dist)) nextCities

-- Main TSP function that starts from an initial city
travelSalesFromCity :: RoadMap -> City -> Path
travelSalesFromCity r startCity =
    let allPaths = travelSalesAux r startCity (filter (/= startCity) (cities r)) [startCity] 0
    in case allPaths of
        [] -> []  -- Nenhum caminho encontrado
        _  -> let (path, totalDist) = findMinimum (\(_, d1) (_, d2) -> compare d1 d2) allPaths
              in case distance r (last path) startCity of  -- Tenta encontrar o caminho de volta ao início
                    Nothing -> []  -- Se não houver retorno, retorna caminho vazio
                    Just dist -> path ++ [startCity]  -- Completa o ciclo de retorno

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

gTest4 ::RoadMap
gTest4 = [("0", "1", 1),("1", "3", 1), ("2", "3",1),("0","2",1)]

{-
-- Grafo com 5 cidades em um ciclo
gTest5 :: RoadMap
gTest5 = [("0", "1", 2), ("1", "2", 3), ("2", "3", 4), ("3", "4", 5), ("4", "0", 1)]

-- Grafo com 6 cidades e várias conexões
gTest6 :: RoadMap
gTest6 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30), ("3", "4", 10), ("4", "5", 5), ("5", "0", 10)]

-- Grafo com 4 cidades em uma linha
gTest7 :: RoadMap
gTest7 = [("0", "1", 5), ("1", "2", 10), ("2", "3", 15)]

-- Grafo com 7 cidades e conexões aleatórias
gTest8 :: RoadMap
gTest8 = [("0", "1", 7), ("0", "2", 9), ("0", "5", 14), ("1", "2", 10), ("1", "3", 15), ("2", "3", 11), ("2", "5", 2), ("3", "4", 6), ("4", "5", 9), ("4", "6", 3), ("5", "6", 8)]

-- Grafo com 8 cidades e conexões densas
gTest9 :: RoadMap
gTest9 = [("0", "1", 1), ("0", "2", 2), ("0", "3", 3), ("0", "4", 4), ("0", "5", 5), ("0", "6", 6), ("0", "7", 7), ("1", "2", 1), ("1", "3", 2), ("1", "4", 3), ("1", "5", 4), ("1", "6", 5), ("1", "7", 6), ("2", "3", 1), ("2", "4", 2), ("2", "5", 3), ("2", "6", 4), ("2", "7", 5), ("3", "4", 1), ("3", "5", 2), ("3", "6", 3), ("3", "7", 4), ("4", "5", 1), ("4", "6", 2), ("4", "7", 3), ("5", "6", 1), ("5", "7", 2), ("6", "7", 1)]

-- Grafo com 9 cidades e algumas conexões
gTest10 :: RoadMap
gTest10 = [("0", "1", 4), ("0", "2", 8), ("1", "2", 11), ("1", "3", 8), ("2", "4", 7), ("3", "4", 2), ("3", "5", 4), ("4", "5", 14), ("4", "6", 9), ("5", "6", 10), ("6", "7", 2), ("7", "8", 6), ("7", "0", 1), ("8", "2", 7), ("8", "6", 6)]
-}