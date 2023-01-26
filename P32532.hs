divisors :: Int -> [Int]
divisors a = filter (\x-> (mod a x) == 0) [1..a]

nbDivisors :: Int -> Int
nbDivisors = length . divisors

moltCompost :: Int -> Bool
moltCompost a= null [ x| x<-[1..(a-1)], (nbDivisors x) >= l]
    where l = nbDivisors a