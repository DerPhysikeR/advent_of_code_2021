import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

getCompletion :: Map Char Char -> [Char] -> [Char] -> Either Char [Char]
getCompletion charMap stack [] = Right stack
getCompletion charMap stack (x:xs)
    | M.member x charMap = getCompletion charMap (newX : stack) xs
    | x == head stack = getCompletion charMap (tail stack) xs
    | otherwise = Left x
    where newX = fromJust (M.lookup x charMap)

getCorruptionScore :: Map Char Char -> String -> Int
getCorruptionScore charMap parens = case getCompletion charMap [] parens of
    Left ')' -> 3
    Left ']' -> 57
    Left '}' -> 1197
    Left '>' -> 25137
    _ -> 0

readInput :: String -> IO [String]
readInput = fmap lines . readFile

main :: IO ()
main = do
    lin <- readInput "input.txt"
    let parens = M.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
    print (sum $ map (getCorruptionScore parens) lin)
