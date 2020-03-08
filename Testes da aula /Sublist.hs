sublist x y ls 
    | x < 0 && y < 0 = drop (length ls + x) (take (length ls + y) ls)
    | x < 0 && y > 0 = drop (length ls + x) (take y ls)   
    | x > 0 && y < 0 = drop x (take (length ls + y) ls)   
    | otherwise = drop x (take y ls)   