import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Point = Point Int Int deriving (Show, Eq)

instance Ord Point where
    (Point row1 col1) `compare` (Point row2 col2)
        | col1 == col2 = row1 `compare` row2
        | otherwise = col1 `compare` col2

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

parseInput :: String -> Map Point Int
parseInput string = toMap [[(Point row col, digitToInt letter) | (col, letter) <- enumerate line] | (row, line) <- enumerate lines']
    where lines' = lines string
          toMap = M.fromList . concat

readInput :: String -> IO (Map Point Int)
readInput = fmap parseInput . readFile

getNeighbors :: Point -> [Point]
getNeighbors (Point row col) = [Point (row + dr) (col + dc) | dr <- d, dc <- d, dr /= 0 || dc /= 0]
    where d = [-1, 0, 1]

findFlashing :: Map Point Int -> [Point]
findFlashing m = [point |(point, energy) <- M.toList m, energy > 9]

incrementPoints :: Map Point Int -> [Point] -> Map Point Int
incrementPoints = foldl (\acc x -> M.insertWith (+) x 1 acc)

_evolve :: Map Point Int -> Set Point -> [Point] -> Map Point Int
_evolve m _ [] = M.map (\x -> if x > 9 then 0 else x) m
_evolve m flashed neighborsToAddTo = _evolve newM newFlashed newNeighborsToAddTo
    where newM = incrementPoints m neighborsToAddTo
          newlyFlashing = [p | (p, energy) <- M.toList newM, S.notMember p flashed, energy > 9]
          newFlashed = S.union flashed (S.fromList newlyFlashing)
          newNeighborsToAddTo = concat [[n | n <- getNeighbors nf, S.notMember n newFlashed, M.member n m] | nf <- newlyFlashing]

evolve :: Map Point Int -> Map Point Int
evolve m = _evolve m S.empty (M.keys m)

countFlashed :: Map Point Int -> Int
countFlashed m = length . filter (\(_, energy) -> energy == 0) $ M.toList m

main :: IO ()
main = do
    octopi <- readInput "input.txt"
    print (sum . take 101 $ [countFlashed m | m <- iterate evolve octopi])
    print (length $ takeWhile (\m -> countFlashed m < length m) (iterate evolve octopi))
