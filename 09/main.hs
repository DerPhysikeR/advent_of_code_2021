import Data.Maybe (catMaybes, isNothing)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (sort)

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

_getBasinAroundLowPoint :: Map Point Int -> Set Point -> [Point] -> Set Point -> Set Point
_getBasinAroundLowPoint heightMap basin [] checked = basin
_getBasinAroundLowPoint heightMap basin (p:rest) checked
    | isNothing height = _getBasinAroundLowPoint heightMap basin rest newChecked
    | height == Just 9 = _getBasinAroundLowPoint heightMap basin rest newChecked
    | otherwise = _getBasinAroundLowPoint heightMap newBasin (rest ++ neighbors) newChecked
    where newChecked = S.insert p checked
          height = M.lookup p heightMap
          newBasin = S.insert p basin
          neighbors = filter (`S.notMember` checked) . filter (`M.member` heightMap) $ getNeighbors p

getBasinAroundLowPoint :: Map Point Int -> Point -> Set Point
getBasinAroundLowPoint heightMap lowPoint = _getBasinAroundLowPoint heightMap basin neighbors checked
    where lowPointSet = S.singleton lowPoint
          neighbors = getNeighbors lowPoint
          basin = lowPointSet
          checked = lowPointSet

getBasins :: Map Point Int -> [Set Point]
getBasins heightMap = map (getBasinAroundLowPoint heightMap) (getLowPoints heightMap)

getProductOf3LargestBasins :: Map Point Int -> Int
getProductOf3LargestBasins = product . take 3 . reverse . sort . map length . getBasins

main :: IO ()
main = do
    content <- readFile "input.txt"
    let heightMap = parseInput content
    print (getRiskLevel heightMap)
    print (getProductOf3LargestBasins heightMap)
