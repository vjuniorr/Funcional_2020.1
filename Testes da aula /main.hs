-- unique
unique [] = []
unique (x:xs) 
    | x `elem` xs = unique xs
    | otherwise = x:unique xs
{-Ordem errada-}

-- quadperf
quadperf x =
    x `elem` [y*y | y <- [1..x]]

-- qsort
qsort [] = []
qsort (x:xs) =
    let menores = qsort [y | y <- xs, y <= x]
        maiores = qsort [z | z <- xs, z > x]
    in  menores ++ [x] ++ maiores
    
-- merge 
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys