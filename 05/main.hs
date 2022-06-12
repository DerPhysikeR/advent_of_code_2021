import Data.List.Split (splitOn)
import Data.Map (fromListWith, toList)

data Point = Point Int Int deriving (Show, Eq)
data Line = Line Point Point deriving (Show)

instance Ord Point where
    (Point x1 y1) `compare` (Point x2 y2)
        | y1 == y2 = x1 `compare` x2
        | otherwise = y1 `compare` y2

parsePoint :: String -> Point
parsePoint string = Point (head parts) (last parts)
    where parts = map read (splitOn "," string)

parseLine :: String -> Line
parseLine string = Line (head parts) (last parts)
    where parts = map parsePoint (splitOn " -> "  string)

parseInput :: String -> [Line]
parseInput string = map parseLine (lines string)

range :: Int -> Int -> [Int]
range start end
    | start < end = [start..end]
    | start == end = repeat start
    | otherwise = [start,start-1..end]

drawLine :: Line -> [Point]
drawLine (Line (Point xs ys) (Point xe ye)) = [Point x y | (x, y) <- zip (range xs xe) (range ys ye)]

countHits :: [Line] -> [(Point, Int)]
countHits lines = toList (fromListWith (+) [(point, 1) | point <- points])
    where points = concatMap drawLine lines

countOverlappingPoints :: [Line] -> Int
countOverlappingPoints lines = length (filter (\(_, count) -> count >= 2) (countHits lines))

isOrtho :: Line -> Bool
isOrtho (Line (Point xs ys) (Point xe ye)) = xs == xe || ys == ye

main :: IO ()
main = do
    content <- readFile "input.txt"
    let lines = parseInput content
    print (countOverlappingPoints (filter isOrtho lines))
    print (countOverlappingPoints lines)
