import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

parseLine :: String -> (String, String)
parseLine line = (head parts, last parts)
    where parts = splitOn "-" line

parseInput :: String -> Map String [String]
parseInput fileContent = foldr (\(start, end) acc -> M.insertWith (++) start [end] acc) M.empty allConnections
    where lines' = lines fileContent
          connections = map parseLine lines'
          reversedConnections = map (\(x, y) -> (y, x)) connections
          allConnections = connections ++ reversedConnections

isSmallCave :: String -> Bool
isSmallCave = all isLower

twiceVisited :: String -> [String] -> Bool
twiceVisited = elem

cantVisitSmallCaveTwice :: String -> [String] -> Bool
cantVisitSmallCaveTwice location path = isSmallCave location && twiceVisited location path

onlyUniqueElements :: [String] -> Bool
onlyUniqueElements [] = True
onlyUniqueElements (x:xs) = notElem x xs && onlyUniqueElements xs

cantVisitMoreThanOneSmallCaveTwice :: String -> [String] -> Bool
cantVisitMoreThanOneSmallCaveTwice "end" [] = False
cantVisitMoreThanOneSmallCaveTwice "end" _ = True
cantVisitMoreThanOneSmallCaveTwice location path
    | not isSmall = False
    | allUnique = False
    | location `notElem` path = False
    | otherwise = True
    where isSmall = isSmallCave location
          smallCaves = filter (all isLower) path
          allUnique = onlyUniqueElements smallCaves

_findPaths :: Map String [String] -> (String -> [String] -> Bool) -> [String] -> String -> [Maybe [String]]
_findPaths m cantVisit path location
    | location == "start" = [Just currentPath]
    | cantVisit location path = [Nothing]
    | otherwise = concatMap (_findPaths m cantVisit currentPath) availableCaves
    where availableCaves = M.findWithDefault [] location m
          currentPath = location : path

readInput :: String -> IO (Map String [String])
readInput = fmap parseInput . readFile

findPaths :: Map String [String] -> (String -> [String] -> Bool) -> [[String]]
findPaths m cantVisit = catMaybes $ _findPaths m cantVisit [] "end"

main :: IO ()
main = do
    connections <- readInput "input.txt"
    print (length (findPaths connections cantVisitSmallCaveTwice))
    print (length (findPaths connections cantVisitMoreThanOneSmallCaveTwice))
