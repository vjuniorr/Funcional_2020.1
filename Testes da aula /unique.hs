unique [] = []
unique (x:xs) 
    | x `elem` xs = unique xs
    | otherwise = [x]: unique xs