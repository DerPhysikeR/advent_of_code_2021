import Data.Either (lefts, rights)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

getCompletion :: [Char] -> [Char] -> Either Char [Char]
getCompletion stack [] = Right stack
getCompletion stack (x:xs)
    | M.member x charMap = getCompletion (newX : stack) xs
    | x == head stack = getCompletion (tail stack) xs
    | otherwise = Left x
    where charMap = M.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
          newX = fromJust (M.lookup x charMap)

getCorruptionScore :: Char -> Int
getCorruptionScore x = fromJust (M.lookup x scoreMap)
    where scoreMap = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

getCompletionScore :: [Char] -> Int
getCompletionScore = foldl (\acc x -> fromJust (M.lookup x scoreMap) + (5 * acc)) 0
    where scoreMap = M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

getOddMedian :: [Int] -> Int
getOddMedian xs = sort xs !! m
    where m = div (length xs) 2

readInput :: String -> IO [String]
readInput = fmap lines . readFile

main :: IO ()
main = do
    lin <- readInput "input.txt"
    let completions = map (getCompletion []) lin
    print (sum . map getCorruptionScore . lefts $ completions)
    print (getOddMedian . map getCompletionScore . rights $ completions)
