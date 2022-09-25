eql :: [Int] -> [Int] -> Bool
eql xs ys
    |length xs /= length ys = False
    |otherwise = (and (zipWith (==) xs ys))

prod :: [Int] -> Int
prod xs = (foldr (*) 1 xs)

prodOfEvens :: [Int]->Int
prodOfEvens xs= (foldr (*) 1 (filter even xs))

powersOf2 :: [Int]
powersOf2 =iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys= (foldl (+) 0 (zipWith (*) xs ys)) 
