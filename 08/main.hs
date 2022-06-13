import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequences (sortOn)

data Entry = Entry [Set Char] [Set Char] deriving (Show)

parseWords :: String -> [Set Char]
parseWords string = [Set.fromList word | word <- words string]

parseInput :: String -> [Entry]
parseInput string = [let parts = splitOn " | " line in parseEntry parts | line <- lines string]
    where parseEntry parts = Entry (parseWords (head parts)) (parseWords (last parts))

count :: [Int] -> [Int] -> Int
count xs stuff = foldr (\x -> (+) (length (filter (==x) stuff))) 0 xs

count1478inOutput :: [Entry] -> Int
count1478inOutput = foldr (\(Entry _ out) -> (+) (count [2, 3, 4, 7] (map length out))) 0

removeItems :: Eq a => [a] -> [a] -> [a]
removeItems xs list =  foldl (\list x -> filter (/=x) list) list xs

_getMapping :: [Set Char] -> Map (Set Char) Int
_getMapping (one:seven:four:rest) = Map.fromList (zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..])
    where lenfive = filter (\x -> length x == 5) rest
          lensix = filter (\x -> length x == 6) rest
          eight = last rest
          six = head (filter (Set.difference eight seven `Set.isSubsetOf`) lensix)
          nine = head (filter (four `Set.isSubsetOf`) lensix)
          zero = head (removeItems [six, nine] lensix)
          three = head (filter (one `Set.isSubsetOf`) lenfive)
          five = head (filter (`Set.isSubsetOf` six) lenfive)
          two = head (removeItems [three, five] lenfive)

getMapping :: [Set Char] -> Map (Set Char) Int
getMapping input = _getMapping (sortOn length input)

toNum :: [Int] -> Int
toNum xs = sum [(xs!!i)*10^(l - i) | i <- [0..l]]
    where l = length xs - 1

decipher :: Map (Set Char) Int -> [Set Char] -> Int
decipher mapping output = toNum (map (\key -> Map.findWithDefault 0 key mapping) output)

addUpOutputs :: [Entry] -> Int
addUpOutputs = foldr (\(Entry input output) -> (+) (decipher (getMapping input) output)) 0

main :: IO ()
main = do
    content <- readFile "input.txt"
    let entries = parseInput content
    print (count1478inOutput entries)
    print (addUpOutputs entries)
