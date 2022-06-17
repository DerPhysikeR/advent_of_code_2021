import Data.List.Extra (zip4)

parseInput :: String -> [[Int]]
parseInput string = map readLine (lines string)
  where
    readLine = map (read . (: ""))

getLeftNeihbors :: [Int] -> [[Int]]
getLeftNeihbors xs = [] : [[x] | x <- init xs]

getRightNeighbors :: [Int] -> [[Int]]
getRightNeighbors xs = [[x] | x <- tail xs] ++ [[]]

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

getNeighbors2D :: [[Int]] -> [[[Int]]]
getNeighbors2D xs = [[ r ++ u ++ l ++ d | (r, u, l, d) <- zip4 hright hup hleft hdown] | (hright, hup, hleft, hdown) <- zip4 right up left down]
    where left = map getLeftNeihbors xs
          right = map getRightNeighbors xs
          transposed = transpose xs
          up = transpose $ map getLeftNeihbors transposed
          down = transpose $ map getRightNeighbors transposed

findLowPoints :: [[Int]] -> [Int]
findLowPoints xs = [p | (p, n) <- zip (concat xs) (concat neighbors), all (>p) n]
    where neighbors = getNeighbors2D xs

getRiskLevel :: [[Int]] -> Int
getRiskLevel = sum . map (+1) . findLowPoints

main :: IO ()
main = do
    content <- readFile "input.txt"
    let heightMap = parseInput content
    print (getRiskLevel heightMap)
