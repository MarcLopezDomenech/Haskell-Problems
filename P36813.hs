import Data.List

degree :: Eq a => [(a, a)] -> a -> Int
degree [] y = 0
degree ((a,b):xs) y
    |a==y ||b==y = (degree xs y) +1
    |otherwise = degree xs y

degree' :: Eq a => [(a, a)] -> a -> Int
--degree' xs y = foldl (\(q,z)-> if q == y || z == y then (\x->x+1) else (\x->x)) 0 xs
degree' xs y = foldl (+) 0 (map (\(q,z)-> if q == y || z == y then 1 else 0) xs)

neighbors :: Ord a => [(a, a)] -> a -> [a] 
neighbors xs y = sort $ map (\(q,z)-> if q == y then z else q) (filter (\(q,z)-> q == y || z==y) xs)