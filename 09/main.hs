import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as M

data Point = Point Int Int deriving (Show, Eq)

instance Ord Point where
    (Point x1 y1) `compare` (Point x2 y2)
        | y1 == y2 = x1 `compare` x2
        | otherwise = y1 `compare` y2

enumerate = zip [0..]

melt :: [[Int]] -> Map Point Int
melt xs = M.fromList $ concat $ [[(Point i j, height) | (j, height) <- enumerate row] | (i, row) <- enumerate xs]

parseInput :: String -> Map Point Int
parseInput string = melt $ map readLine (lines string)
  where
    readLine = map (read . (: ""))

getNeighbors :: Point -> [Point]
getNeighbors (Point row col) = [Point (row - 1) col, Point (row + 1) col, Point row (col - 1), Point row (col + 1)]

getNeighborHeights :: Map Point Int -> Point -> [Int]
getNeighborHeights heightMap point = catMaybes $ [M.lookup neighbor heightMap | neighbor <- getNeighbors point]

getLowPoints :: Map Point Int -> [Point]
getLowPoints heightMap = [point | (point, height) <- M.toList heightMap, all (>height) (getNeighborHeights heightMap point)]

getRiskLevel :: Map Point Int -> Int
getRiskLevel heightMap = sum $ map (+1) lowHeights
    where lowHeights = catMaybes [M.lookup lowPoint heightMap | lowPoint <- getLowPoints heightMap]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let heightMap = parseInput content
    print (getRiskLevel heightMap)
