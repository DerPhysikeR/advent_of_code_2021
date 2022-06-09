data Bit = O | I deriving (Eq, Show)

parseBinaryNumber :: String -> [Bit]
parseBinaryNumber [] = []
parseBinaryNumber ('0':xs) = O : parseBinaryNumber xs
parseBinaryNumber ('1':xs) = I : parseBinaryNumber xs

toDecimal :: [Bit] -> Int
toDecimal [] = 0
toDecimal (O:bs) = toDecimal bs
toDecimal (I:bs) = 2 ^ length bs + toDecimal bs

invert :: [Bit] -> [Bit]
invert [] = []
invert (O:xs) = I : invert xs
invert (I:xs) = O : invert xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

mostCommonBit :: [Bit] -> Bit
mostCommonBit bits
    | ocount > icount = O
    | ocount < icount = I
    where ocount = count O bits
          icount = count I bits

mostCommonBits :: [[Bit]] -> [Bit]
mostCommonBits l@(x:xs)
    | null x = []
    | otherwise = mostCommonBit [head y | y <- l] : mostCommonBits [tail y | y <- l]

getPowerConsumption :: [[Bit]] -> Int
getPowerConsumption xs = toDecimal gr * toDecimal (invert gr)
    where gr = mostCommonBits xs

main :: IO ()
main = do
    content <- readFile "input.txt"
    let numbers = map parseBinaryNumber (words content)
    print (getPowerConsumption numbers)

