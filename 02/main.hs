data Position = Position Int Int deriving (Show)
data Direction = Forward | Up | Down
data Command = Command Direction Int

parseCommand :: String -> Command
parseCommand ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : rest) = Command Forward (read rest)
parseCommand ('u' : 'p' : ' ' : rest) = Command Up (read rest)
parseCommand ('d' : 'o' : 'w' : 'n' : ' ' : rest) = Command Down (read rest)

followCourse :: Position -> [Command] -> Position
followCourse p [] = p
followCourse (Position dist depth) (c : cs) =
    case c of
        (Command Forward x) -> followCourse (Position (dist + x) depth) cs
        (Command Up x) -> followCourse (Position dist (depth - x)) cs
        (Command Down x) -> followCourse (Position dist (depth + x)) cs

followCourseCorrectly :: Position -> Int -> [Command] -> Position
followCourseCorrectly p a [] = p
followCourseCorrectly p@(Position dist depth) aim (c : cs) =
    case c of
        (Command Forward x) -> followCourseCorrectly (Position (dist + x) (depth + aim * x)) aim cs
        (Command Up x) -> followCourseCorrectly p (aim - x) cs
        (Command Down x) -> followCourseCorrectly p (aim + x) cs

main :: IO ()
main = do
    content <- readFile "input.txt"
    let commands = map parseCommand (lines content)
    print (followCourse (Position 0 0) commands)
    print (followCourseCorrectly (Position 0 0) 0 commands)
