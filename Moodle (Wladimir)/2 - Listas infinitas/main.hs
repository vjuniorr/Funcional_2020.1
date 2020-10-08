-- kolakoski --
kolakoski :: [Int]
kolakoski = 1:2: drop 2 (concat . zipWith replicate kolakoski . cycle $ [1, 2])

-- hamming --
{- hamming :: [Integer]
hamming = [x | x <- [1 .. ] , x `elem` mescla3]

mescla3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
mescla3 xs [] [] = xs
mescla3 [] ys [] = ys
mescla3 [] [] zs = zs
mescla3 (x:xs) (y:ys) [] 
    | x `elem` ys = mescla3 xs ys
    | otherwise = x ++ y ++ mescla3 xs ys
mescla3 (x:xs) [] (z:zs)
    | x `elem` zs = mescla3 xs zs
    | otherwise = x ++ z ++ mescla3 xs zs
mescla3 [] ys zs 
mescla3 (x:xs) (y:ys) (z:zs) 
    |  -}

-- collatz --
collatz :: Integer -> [Integer]
collatz x 
    | x == 1 = [1]
    |otherwise = [x] ++ collatz (seguinte x)

seguinte :: Integer -> Integer
seguinte 1 = 1
seguinte n 
    | n `mod` 2 == 0 = (n `div` 2)
    | otherwise = (n*3)+1

-- fechoKleene --
{- fechoKleene :: [a] -> [[a]]
fechoKleene xs =  -}

-- goldbach -- 
goldbach :: Int -> [(Int,Int,Int)]
goldbach n = [(n,x,y) | x <- primesR 2 (n-1),let y = n-x, isPrime y]

primesR a b = 
    takeWhile (<= b) $ dropWhile (< a) $ sieve [2..]
    where sieve (n:ns) = n:sieve [ m | m <- ns, m `mod` n /= 0 ]


divisores:: Int -> Int -> [Int] -> [Int]
divisores 0 n x = x
divisores divisor n x
    | n `mod` divisor /= 0 = divisores (divisor-1) n x
    | otherwise = [divisor] ++ (divisores (divisor-1) n x)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime k 
    | length(divisores k k []) == 2 = True
    | otherwise = False

-- primosPalindromos --
primosPalindromo :: [Int]
primosPalindromo = [ x | x <- [2 .. ],isPrime x, palindromo (show x) ]

palindromo:: String -> Bool
palindromo [] = True
palindromo x 
    | x == reverse x = True
    | otherwise = False

-- primosGemeos --
primosGemeos :: [(Int, Int)]
primosGemeos = [(y,x) | x <- [2 .. ], let y = x - 2, isPrime x, isPrime y]
