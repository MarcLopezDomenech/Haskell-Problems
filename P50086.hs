data Queue a = Queue [a] [a]
     deriving (Show)
 
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push n (Queue x y) = Queue x (n:y)

pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue [] bs) = Queue (reverse (init bs)) []
pop (Queue (x:xs) y) = Queue xs y

top :: Queue a -> a
top (Queue [] bs) = last bs
top (Queue (x:_) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a)
     where (Queue as1 bs1) == (Queue as2 bs2) = (as1 ++ reverse bs1) == (as2 ++ reverse bs2)

instance Functor Queue where
     fmap  f  (Queue as1 bs1) = (Queue (fmap (f) as1) (fmap (f) bs1))

translation :: Num b => b -> Queue b -> Queue b 
translation  a b= fmap (+a) b;

q2l :: (Queue a) -> [a]
q2l (Queue as bs) = (as ++ (reverse bs))

instance Applicative Queue
    where
        pure x = (Queue [x] [])
        f <*> queue = (Queue ((q2l f) <*> (q2l queue)) [])
instance Monad Queue
    where
        return x = (Queue [x] [])
        queue >>= f = (Queue l [])
            where
                l = (q2l queue) >>= (q2l . f)
     
kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f queue = do
     q <- queue
     if (f q) then return q
     else (Queue [] [])




