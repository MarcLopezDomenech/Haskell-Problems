insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert (x:xs) n
    |n<x =[n]++[x]++xs
    |otherwise =[x]++ insert xs n

isort :: [Int] -> [Int]
isort []=[]
isort (x:xs) =insert (isort xs) x

remove :: [Int]->Int->[Int]
remove (x:xs) n
    |x==n = xs
    |otherwise = [x] ++ remove xs n

ssort :: [Int]->[Int]
ssort []=[]
ssort xs = [n] ++ ssort (remove xs n)
    where n = minimum xs

merge :: [Int]->[Int]->[Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    |x< y = [x] ++ merge xs ([y] ++ ys)
    |otherwise  =[y] ++ merge ys ([x] ++ xs)

msort [] = []
msort [element] = [element]
msort list = merge (msort (fst sliced)) (msort (snd sliced))
    where sliced = split list

split [] = ([],[])
split list = splitAt (((length list) + 1) `div` 2) list

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort (smaller xs x)) ++ (x:(qsort (greater xs x)))

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:xs) = (genQsort (smaller xs x)) ++ (x:(genQsort (greater xs x)))

smaller :: Ord a => [a] -> a -> [a]
smaller [] _ = []
smaller list x = [n | n <- list, n <= x]

greater :: Ord a => [a] -> a -> [a]
greater [] _ = []
greater list x = [n | n <- list, n > x]