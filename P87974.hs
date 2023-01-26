main = do
    xs <- getLine
    if last xs == 'a' || last xs == 'A' then putStrLn ("Hola maca!")
    else putStrLn ("Hola maco!")