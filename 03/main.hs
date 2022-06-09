data Bit = O | I deriving (Eq, Show)

parseBinaryNumber :: String -> [Bit]
parseBinaryNumber [] = []
parseBinaryNumber ('0':xs) = O : parseBinaryNumber xs
parseBinaryNumber ('1':xs) = I : parseBinaryNumber xs

toDecimal :: [Bit] -> Int
toDecimal [] = 0
toDecimal (O:bs) = toDecimal bs
toDecimal (I:bs) = 2 ^ length bs + toDecimal bs

invertBit :: Bit -> Bit
invertBit O = I
invertBit I = O

invertBits :: [Bit] -> [Bit]
invertBits = map invertBit

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

mostCommonBit :: [Bit] -> Bit
mostCommonBit bits
    | count O bits > count I bits = O
    | otherwise = I

mostCommonBits :: [[Bit]] -> [Bit]
mostCommonBits l@(x:xs)
    | null x = []
    | otherwise = mostCommonBit [head y | y <- l] : mostCommonBits [tail y | y <- l]

getPowerConsumption :: [[Bit]] -> Int
getPowerConsumption xs = toDecimal gr * toDecimal (invertBits gr)
    where gr = mostCommonBits xs

getOxygenGeneratorRating :: [[Bit]] -> Int -> Int
getOxygenGeneratorRating [x] _ = toDecimal x
getOxygenGeneratorRating xs index = getOxygenGeneratorRating (filter (\y -> y!!index == mcb) xs) (index + 1)
    where mcb = mostCommonBit [y!!index | y <- xs]

getCO2ScrubberRating :: [[Bit]] -> Int -> Int
getCO2ScrubberRating [x] _ = toDecimal x
getCO2ScrubberRating xs index = getCO2ScrubberRating (filter (\y -> y!!index == lcb) xs) (index + 1)
    where lcb = invertBit (mostCommonBit [y!!index | y <- xs])

getLifeSupportRating :: [[Bit]] -> Int
getLifeSupportRating xs = getOxygenGeneratorRating xs 0 * getCO2ScrubberRating xs 0

main :: IO ()
main = do
    content <- readFile "input.txt"
    let numbers = map parseBinaryNumber (words content)
    print (getPowerConsumption numbers)
    print (getLifeSupportRating numbers)
