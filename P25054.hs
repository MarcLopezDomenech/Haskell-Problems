myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) =1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (first:sec:list)
    |first>=sec =myMaximum(first:list)
    |first<sec =myMaximum(sec:list)

average :: [Int] -> Float
average list = fromIntegral(suma(list))/ fromIntegral(myLength(list))
    where  
        suma :: [Int] -> Int
        suma [] = 0
        suma (x:xs) = x + suma xs

buildPalindrome :: [Int] -> [Int]
buildPalindrome list = reverse(list) ++ list
    where
        reverse :: [Int] -> [Int]
        reverse []=[]
        reverse (x:xs) = (reverse(xs)++[x])

remove :: [Int]-> [Int]-> [Int]
remove [] _ =[]
remove (x:xs) list = saca x list ++ remove xs list
    where
        saca :: Int -> [Int] -> [Int]
        saca n [] = [n]
        saca n (first:list)
            |n ==first =[]
            |otherwise = saca n list

flatten :: [[Int]] -> [Int]
flatten [] = [] 
flatten (x:xs) =x ++ flatten xs

oddsNevens::[Int]->([Int],[Int])
oddsNevens []= ([],[])
oddsNevens (x:xs)
    |mod x 2==0 = (fst(f),[x] ++ snd(f))
    |otherwise = ([x] ++ fst(f),snd(f))
        where f = oddsNevens xs

primeDivisors :: Int->[Int]
primeDivisors n =divisors n 2 []
    where 
        divisors :: Int->Int->[Int]->[Int]
        divisors x n xs
            |x<n =xs
            |mod x n == 0 = divisors (dividir x n) (n+1) (xs ++ [n]) 
            |otherwise = divisors x (n+1) xs
                where
                    dividir :: Int->Int->Int
                    dividir x n
                        |mod x n ==0 =dividir (div x n) n
                        |otherwise =x
    
