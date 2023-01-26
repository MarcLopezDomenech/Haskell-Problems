roman2int :: String -> Int
roman2int [x] = (val x)
roman2int (x:xs)
    |(val x)<(val (head xs)) = (roman2int xs) - (val x)
    |otherwise = (roman2int xs) + (val x)

roman2int' :: String -> Int
roman2int' xs = foldl (\x (y,z) -> if y >= z then x+y else x-y) 0 (zipWith (,) l ((tail l) ++ [0]))
    where l = map val xs

val :: Char -> Int
val a
    |a == 'I' =1
    |a == 'V' =5
    |a == 'X' =10
    |a == 'L' =50
    |a == 'C' =100
    |a == 'D' =500
    |a == 'M' =1000

arrels :: Float -> [Float]
arrels x = iterate (\y -> 0.5 * (y+(x/y))) x

arrel :: Float -> Float -> Float
arrel x e =snd $ head $ dropWhile (\(y,z)->abs(y-z)>e) (zipWith (,) l (tail l))
    where l = (arrels x)

data LTree a = Leaf a | Node (LTree a) (LTree a)

instance Show a => Show(LTree a) where
    show (Leaf a)= "{"++show a++"}"
    show (Node (a) (b))= "<"++show a++","++show b++">"

build :: [a] -> LTree a
build [x]= Leaf x
build xs = Node (build xs1) (build ys1)
    where (xs1,ys1) = eq [] xs

eq :: [a]->[a]->([a],[a])
eq xs ys 
    |(length xs) >= (length ys) = (xs,ys)
    |otherwise = eq (xs++[head ys]) (tail ys)

zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))
zipLTrees (Leaf a) (Leaf b) = Just (Leaf (a,b))
zipLTrees (Node (a) (b)) (Node (c) (d))= do
    p<-zipLTrees a c
    q<-zipLTrees b d
    Just (Node p q)
zipLTrees _ _ = Nothing