import Data.List.Split (splitOn)
import Data.List.Extra (find)

parseNumbers :: String  -> [Int]
parseNumbers string = map read (words string)

parseBingoGame :: String -> [[Int]]
parseBingoGame string = map parseNumbers (lines string)

parseInput :: String -> ([Int], [[[Int]]])
parseInput string = (map read (splitOn "," (head list)), map parseBingoGame (tail list))
    where list = splitOn "\n\n" string

checkBingo :: [Int] -> [[Int]] -> Bool
checkBingo nums board = _checkRows nums board || _checkCols nums board || _checkDiags nums board

_checkBingo :: [Int] -> [Int] -> Bool
_checkBingo nums = all (`elem` nums)

_checkRows :: [Int] -> [[Int]] -> Bool
_checkRows nums = any (_checkBingo nums)

_checkCols :: [Int] -> [[Int]] -> Bool
_checkCols nums board
    | null (head board) = False
    | otherwise = _checkBingo nums [head x | x <- board] || _checkCols nums [tail x | x <- board]

_checkDiags :: [Int] -> [[Int]] -> Bool
_checkDiags nums board = _checkBingo nums leftDiag || _checkBingo nums rightDiag
    where leftDiag = [(board!!i)!!i | i <- [0..max]]
          rightDiag = [(board!!i)!!i | i <- [max,max-1..0]]
          max = length board - 1

getScore :: [Int] -> [[Int]] -> Int
getScore nums board = last nums * sum (filter (`notElem` nums) (concat board))

checkForWinningBoard :: [Int] -> [[[Int]]] -> Maybe [[Int]]
checkForWinningBoard nums = find (checkBingo nums)

_getFirstWinningBoardScore :: Int -> [Int] -> [[[Int]]] -> Maybe Int
_getFirstWinningBoardScore n nums boards
    | n > length nums = Nothing
    | otherwise = case checkForWinningBoard taken boards of
        Nothing -> _getFirstWinningBoardScore (n + 1) nums boards
        Just winner -> Just (getScore taken winner)
    where taken = take n nums

getFirstWinningBoardScore :: [Int] -> [[[Int]]] -> Maybe Int
getFirstWinningBoardScore = _getFirstWinningBoardScore 0

_getLastWinningBoardScore :: Int -> [Int] -> [[[Int]]] -> Maybe Int
_getLastWinningBoardScore n nums [] = Nothing
_getLastWinningBoardScore n nums [b] = Just (getScore (take n nums) b)
_getLastWinningBoardScore n nums boards = _getLastWinningBoardScore (n + 1) nums newBoards
    where newBoards = filter (not . checkBingo (take n nums)) boards

getLastWinningBoardScore :: [Int] -> [[[Int]]] -> Maybe Int
getLastWinningBoardScore = _getLastWinningBoardScore 0

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (nums, boards) = parseInput content
    print (getFirstWinningBoardScore nums boards)
    print (getLastWinningBoardScore nums boards)
