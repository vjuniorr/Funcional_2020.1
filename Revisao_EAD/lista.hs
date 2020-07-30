-- menorDeDois
menorDeDois x y = min x y

-- menorDeTres
menorDeTres x y z  
    | x < y && x < z = x
    | y < x && y < z = y
    | otherwise = z

-- fatorial
fatorial 0 = 1
fatorial x =
    x * fatorial(x - 1)

-- fibonacci
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = 
    fibonacci (x - 1) + fibonacci(x - 2)

-- elemento
elemento x xs = 
    xs !! x

-- pertence
pertence x xs =
    x `elem` xs

-- total
total xs =
    sum [1 | _ <- xs]

-- maior
maior xs = 
    maioraux xs 0

maioraux [] z = z
maioraux xs z 
    | head xs > z = maioraux (tail xs) (head xs)
    | otherwise = maioraux (tail xs) z

-- frequencia
frequencia x [] = 0
frequencia x xs 
    | head xs == x = 1 + frequencia x (tail xs)
    | otherwise = frequencia x (tail xs)

-- unico 
unico x xs 
    | frequencia x xs == 1 = True
    | otherwise = False

-- maioresQue
maioresQue x [] = []
maioresQue x xs 
    | x < head xs = head xs:maioresQue x (tail xs)
    | otherwise = maioresQue x (tail xs)

-- concat
concate [] [] = []
concate xs ys =
    xs++ys

-- calda
calda xs = tail xs

-- corpo 
corpo xs = init xs

-- unique
unique [] = []
unique (x:xs) 
    | x `elem` xs = unique xs
    | otherwise = x:unique xs

-- alter
alter n =
    concat [[x,-x] | x <- [1 .. n]] 

-- reverso 
reverso xs = reverse xs

-- divide 
divide x xs =
    (take x xs, drop x xs)

-- intercal 
intercal [] ys = ys
intercal xs [] = xs
intercal (x:xs) (y:ys) =
    x:y:intercal xs ys

-- uniao
uniao xs ys = 
    xs ++ [y | y <- ys, y `notElem` xs]

-- intersec
intersec xs ys =
    [x | x <- xs, x `elem` xs && x `elem` ys]

-- sequencia 
sequencia x y = 
    [y .. (y + x - 1)]

