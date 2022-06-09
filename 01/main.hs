countDepthIncreases :: [Int] -> Int
countDepthIncreases (a:b:as)
    | a < b = 1 + r
    | otherwise = r
    where r = countDepthIncreases (b : as)
countDepthIncreases _ = 0

sumSlidingWindow :: [Int] -> [Int]
sumSlidingWindow (a:b:c:xs) = (a + b + c) : sumSlidingWindow (b:c:xs)
sumSlidingWindow _ = []

main :: IO ()
main = do
    content <- readFile "input.txt"
    let depths = map read (words content)
    print (countDepthIncreases depths)
    print (countDepthIncreases $ sumSlidingWindow depths)
