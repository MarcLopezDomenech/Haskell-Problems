flatten :: [[Int]] -> [Int]
flatten xs = foldr (++) [] xs

myLength :: String -> Int
myLength string = sum $ map (const 1) string

myReverse :: [Int] ->[Int]
myReverse list= foldl (\xs x->x:xs) [] list

countIn :: [[Int]] -> Int -> [Int]
countIn list n = map (\xs ->length(filter (== n) xs)) list

firstWord :: String -> String
firstWord text = takeWhile (/= ' ') (dropWhile (== ' ') text)
