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

_findPaths :: Map String [String] -> [String] -> String -> [Maybe [String]]
_findPaths m path location
    | location == "start" = [Just currentPath]
    | isSmallCave location && location `elem` path = [Nothing]
    | otherwise = concatMap (_findPaths m currentPath) availableCaves
    where availableCaves = M.findWithDefault [] location m
          currentPath = location : path

readInput :: String -> IO (Map String [String])
readInput = fmap parseInput . readFile

findPaths :: Map String [String] -> [[String]]
findPaths m = catMaybes $ _findPaths m [] "end"

main :: IO ()
main = do
    connections <- readInput "input.txt"
    print (length (findPaths connections))
