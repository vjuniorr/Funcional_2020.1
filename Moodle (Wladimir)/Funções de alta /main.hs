-- concatenaFold --
concatenaFold :: [[Int]] -> [Int]
concatenaFold [] = []
concatenaFold (x:xs) = foldl(++)x xs

-- inverteFold --
inverteFold :: [Int] -> [Int]
inverteFold xs = foldl (\x z -> z:x)[] xs

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