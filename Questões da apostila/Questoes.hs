-- Maior do melhor
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Lista vazia"
maximum' [x] = x
maximum' (x:xs) 
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Replicate 
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

-- Take 
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- Quick Sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    menorqk ++ [x] ++ maiorqk
    where menorqk = quicksort [y | y<-xs, y <= x]
          maiorqk = quicksort [y | y<-xs, y > x]