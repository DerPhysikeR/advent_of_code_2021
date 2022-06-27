import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)

getCompletion :: [Char] -> [Char] -> Either Char [Char]
getCompletion stack [] = Right stack
getCompletion stack (x:xs)
    | M.member x charMap = getCompletion (newX : stack) xs
    | x == head stack = getCompletion (tail stack) xs
    | otherwise = Left x
    where charMap = M.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
          newX = fromJust (M.lookup x charMap)

getCorruptionScore :: Either Char [Char] -> Int
getCorruptionScore (Left x) = fromJust (M.lookup x scoreMap)
    where scoreMap = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
getCorruptionScore _ = 0

getCompletionScore :: Either Char [Char] -> Maybe Int
getCompletionScore (Right xs) = Just (foldl (\acc x -> fromJust (M.lookup x scoreMap) + (5 * acc)) 0 xs)
    where scoreMap = M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]
getCompletionScore _ = Nothing

getOddMedian :: [Int] -> Int
getOddMedian xs = sort xs !! m
    where m = div (length xs) 2

readInput :: String -> IO [String]
readInput = fmap lines . readFile

main :: IO ()
main = do
    lin <- readInput "input.txt"
    let completions = map (getCompletion []) lin
    print (sum $ map getCorruptionScore completions)
    print (getOddMedian $ mapMaybe getCompletionScore completions)
