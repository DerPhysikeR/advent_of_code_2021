import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput string = map read (splitOn "," (head (lines string)))

calcFuelCost :: Int -> [Int] -> Int
calcFuelCost target crabs = sum [abs (crab - target) | crab <- crabs]

calcNonLinearFuelCost :: Int -> [Int] -> Int
calcNonLinearFuelCost target crabs = sum [let n = abs (target - crab) in div (n * (n + 1)) 2 | crab <- crabs]

calcMinimalFuelCost :: (Int -> [Int] -> Int) -> [Int] -> Int
calcMinimalFuelCost fuelCalculator crabs = minimum [fuelCalculator target crabs | target <- [minimum crabs .. maximum crabs]]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let crabs = parseInput content
    print (calcMinimalFuelCost calcFuelCost crabs)
    print (calcMinimalFuelCost calcNonLinearFuelCost crabs)
