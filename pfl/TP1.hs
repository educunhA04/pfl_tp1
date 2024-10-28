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
        Just rest -> Just (dist + rest) -- Add the distance and continue


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

travelSales :: RoadMap -> Path
travelSales r = case cities r of
    [] -> []
    (startCity:_) -> findShortestPath startCity (cities r)
    where
        findShortestPath :: City -> [City] -> Path
        findShortestPath start cities = case filter (isValidPath cities) (shortestPath r start start) of
            [] -> []
            (path:_) -> path

        isValidPath :: [City] -> Path -> Bool
        isValidPath cities path = length (mynub path) == length cities + 1

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

main :: IO ()
main = do
    print $ cities gTest1  -- Should return ["7","6","8","2","5","0","1","3","4"]
    print $ cities gTest2  -- Should return ["0","1","2","3"]
    print $ areAdjacent gTest1 "0" "1"  -- Should return True
    print $ areAdjacent gTest1 "0" "2"  -- Should return False
    print $ areAdjacent gTest2 "0" "2"  -- Should return True
    print $ distance gTest1 "0" "1"  -- Should return Just 4
    print $ distance gTest1 "0" "2"  -- Should return Nothing
    print $ distance gTest2 "0" "2"  -- Should return Just 15
    print $ adjacent gTest1 "0"  -- Should return [("1",4),("7",8)]
    print $ adjacent gTest1 "7"  -- Should return [("6",1),("8",7),("1",11)]
    print $ pathDistance gTest1 ["0","7","6","5","4"]  -- Should return Just 21
    print $ pathDistance gTest1 ["0","7","1","2","5","4"]  -- Should return Just 27
    print $ pathDistance gTest1 ["0","7","1","2","3"]  -- Should return Nothing
    print $ pathDistance gTest1 ["0","7","1","2","5"]  -- Should return Nothing
    print $ maximuml gTest1  -- Should return 3
    print $ rome gTest1  -- Should return ["7"]
    print $ rome gTest2  -- Should return ["0"]
    print $ isStronglyConnected gTest1  -- Should return False
    print $ isStronglyConnected gTest2  -- Should return True
    print $ shortestPath gTest1 "0" "4"  -- Should return [["0","1","7","6","5","4"],["0","1","2","5","4"],["0","7","6","5","4"],["0","7","8","2","5","4"],["0","7","1","2","5","4"]]
    print $ shortestPath gTest2 "0" "3"  -- Should return [["0","3"]]
    print $ shortestPath gTest3 "0" "3"  -- Should return []7
    print $ travelSales gTest1  -- Should return ["0","1","7","6","5","4","3","2","8"]
    print $ travelSales gTest2  -- Should return ["0","1","3","2"]
    print $ travelSales gTest3  -- Should return ["0","1","3","2"]
