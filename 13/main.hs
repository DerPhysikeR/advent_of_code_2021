import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

data Point = Point Int Int deriving (Eq, Show)

instance Ord Point where
    (Point x1 y1) `compare` (Point x2 y2)
        | y1 == y2 = x1 `compare` x2
        | otherwise = y1 `compare` y2

data Fold = X Int | Y Int deriving (Show)
type Paper = Set Point

takeBefore :: Eq a => a -> [a] -> [a]
takeBefore s = takeWhile (/=s)

takeAfter :: Eq a => a -> [a] -> [a]
takeAfter s [] = []
takeAfter s (x:xs)
    | x == s = xs
    | otherwise = takeAfter s xs

partition :: Eq a => a -> [a] -> ([a], [a])
partition s xs = (takeBefore s xs, takeAfter s xs)

readPoint :: String -> Point
readPoint s = Point (read x) (read y)
    where (x, y) = partition ',' s

readFold :: String -> Maybe Fold
readFold s = case partition '=' s of
    ("fold along x", num) -> Just (X (read num))
    ("fold along y", num) -> Just (Y (read num))
    _ -> Nothing

parseInput :: String -> (Paper, [Fold])
parseInput content = (S.fromList (readPoints a), readFolds b)
    where (a, b) = partition "" (lines content)
          readPoints = map readPoint
          readFolds = mapMaybe readFold

readInput :: String -> IO (Paper, [Fold])
readInput = fmap parseInput . readFile

foldPaper :: Paper -> Fold -> Paper
foldPaper paper (X n) = S.fromList [ if x <= n then Point x y else Point (2 * n - x) y | Point x y <- S.toList paper]
foldPaper paper (Y n) = S.fromList [ if y <= n then Point x y else Point x (2 * n - y) | Point x y <- S.toList paper]

paperSize :: Paper -> (Point, Point)
paperSize paper = (minPoint, maxPoint)
    where xs = [x | Point x _ <- S.toList paper]
          ys = [y | Point _ y <- S.toList paper]
          minPoint = Point (minimum xs) (minimum ys)
          maxPoint = Point (maximum xs) (maximum ys)

prettyShowPaper :: Paper -> String
prettyShowPaper paper = concat [[let point = Point x y in if point `S.member` paper then '█' else '.' | x <- xs] ++ "\n" | y <- ys]
    where (Point minx miny, Point maxx maxy) = paperSize paper
          xs = [minx..maxx]
          ys = [miny..maxy]

main :: IO ()
main = do
    (paper, folds) <- readInput "input.txt"
    print (length (foldPaper paper (head folds)))
    putStrLn (prettyShowPaper (foldl foldPaper paper folds))
