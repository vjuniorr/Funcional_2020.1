-- concatenaFold --
concatenaFold :: [[Int]] -> [Int]
concatenaFold [] = []
concatenaFold (x:xs) = foldl(++)x xs

-- inverteFold --
inverteFold :: [Int] -> [Int]
inverteFold xs = foldl (flip(:)) [] xs

-- paridadeFold --
paridadeFold :: [Bool] -> Bool
paridadeFold xs
    | length(filter (==True) xs) `mod` 2 == 0 = True 
    | otherwise = False

-- duplicarFold -- 
duplicarFold :: String -> String
duplicarFold [] = []
duplicarFold (s:str) = 
    let xs = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']
    in 
        if s `elem` xs then 
                s : filter(== s) xs ++ duplicarFold str
            else 
                s : duplicarFold str

-- filtraAplicaFold --
filtraAplicaFold :: (a->b) -> (a->Bool) -> [a] -> [b]
filtraAplicaFold f p xs = 
    map f (filter p xs)

-- mapFold --
mapFold :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
mapFold f xs = foldr (\x z -> f x : z) [] xs   

-- removeLista --
removeLista :: (Foldable t1, Foldable t2, Eq a) => t2 a -> t1 a -> [a]
removeLista xs ys = foldr (\x z-> if x `notElem` xs then x:z else z ) [] ys

-- acertosFold --
{- acertosFold xs ys = foldr g v [0..tam-1]

  where

  v = 0  

  tam = length xs  -}

-- descompactaFold -- 
descompactaFold :: Foldable t => t (a1, a2) -> ([a1], [a2])
descompactaFold xs = 
    foldr (\(a,b) (xs,ys) -> ([a]++xs, [b]++ys)) ([],[]) xs
