menorDeTres x y z 
  | z < y && z < x = z
  | x < y && x < z = x
  | otherwise = y
            
main = do
    line1 <- getLine
    line2 <- getLine
    line3 <- getLine

    let x = read line1 :: Int
    let y = read line2 :: Int
    let z = read line3 :: Int

    print $ menorDeTres x y z