absValue :: Int -> Int
absValue n= if n>=0 then n else (-n)

power :: Integer -> Integer-> Integer
power _ 0  = 1
power x n
    | even n = y * y
    | otherwise = y * y * x
    where 
        y = power x (n_half)
        n_half = div n 2

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 3 = True
isPrime 2 = True
isPrime n
    | mod n 2 == 0 = False
    | otherwise = isPrime' 3
        where
            isPrime' d
                | mod n d == 0 = False
                | d*d > n = True
                | otherwise = isPrime' (d+2)

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n =(slowFib(n-1)+slowFib(n-2))

quickFib :: Int -> Int
quickFib n = snd (quickFib' n)
    where 
        quickFib' :: Int -> (Int,Int)
        quickFib' 0 = (1,0)
        quickFib' d = (a, a+b)
            where
                (b,a) = quickFib' (d-1)