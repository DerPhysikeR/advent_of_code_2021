import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput content = map read (splitOn "," (head (lines content)))

count :: Int -> [Int] -> Int
count toFind = length . filter (==toFind)

-- update :: Int -> Int
-- update x
--     | x < 0 = 6
--     | otherwise = x

-- evolve :: [Int] -> [Int]
-- evolve xs = [update x | x <- downCounted] ++ extend
--     where downCounted = [x - 1 | x <- xs]
--           minusOnes = count (-1) downCounted
--           extend = replicate minusOnes 8

toFastFishies :: [Int] -> [Int]
toFastFishies fishies = [count n fishies | n <- [0..8]]

fastUpdate :: [Int] -> Int -> Int
fastUpdate xs 8 = head xs
fastUpdate xs 6 = xs!!7 + head xs
fastUpdate xs i = xs!!(i+1)

fastEvolve :: [Int] -> [Int]
fastEvolve xs = [fastUpdate xs i | i <- [0..(length xs - 1)]]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let fishies = parseInput content
    let fastFishies = toFastFishies fishies
    print (sum (iterate fastEvolve fastFishies!!80))
    print (sum (iterate fastEvolve fastFishies!!256))
