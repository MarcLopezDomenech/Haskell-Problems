main = do
    p<-getContents
    let ws=words p
    let w = map (read) (ws)
    let z = foldl (+) 0 w
    putStrLn(show z)