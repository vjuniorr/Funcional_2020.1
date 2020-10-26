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
    |otherwise = x : collatz (seguinte x)

seguinte :: Integer -> Integer
seguinte 1 = 1
seguinte n 
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = (n*3)+1

{-
    collatz recebe um inteiro e retorna uma lista de inteiros, essa lista é determinada da seguinte maneira
    Se o meu x passado for um numero par, o proximo numero da lista vai ser esse numero divido por 2
    Se o meu x passado for um numero impar, o proximo numero da lista vai ser esse numero vezes 3 mais 1
    O numero seguinte é determinado pela função seguinte e retornado para ser concatenado com x
    É realizado esse processo varias vezes até que o numeo passado para collatz seja 1
    Então collatz 138 -> [138,69,208,104,52,26,13,40,20,10,5,16,8,4,2,1]
-}

-- fechoKleene --
{- fechoKleene :: [a] -> [[a]]
fechoKleene xs =  -}

-- goldbach -- 
goldbach :: Int -> [(Int,Int,Int)]
goldbach n = [(n,x,y) | x <- primesR 2 (n-1),let y = n-x, isPrime y]

primesR :: p -> Int -> [Int]
primesR a b = 
    takeWhile (<= b) $ [x | x <- [2 .. ], isPrime x]


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


{-
    goldbach vai retornar uma lista de (n,x,y) on de n é o numero passado e x e y são numeros primos que somados dão n
    primesR vai pegar todos os numeros que são menos que (n-1)
    divisores vai retornar a lista de divisores para verificar se aquele numero é primo
    isPrime verifica se um numero é primo com base na quantidade de divisores, se o numero for primo ele entra na lista de primeR
    Então goldbach 10 -> [(10,3,7),(10,5,5),(10,7,3)]
-}

-- primosPalindromos --
primosPalindromo :: [Int]
primosPalindromo = [ x | x <- [2 .. ],isPrime x, palindromo (show x) ]

palindromo:: String -> Bool
palindromo [] = True
palindromo x 
    | x == reverse x = True
    | otherwise = False

{-
    primosPalindromo retorna uma lista infinitia de numeros primos que são palindromos
    Usando compressão de lista faço uma lista com todos os x de [2 .. ] que são primos, verificados na função isPrime e palindromo
    Então take 10 primosPalindromo -> [2,3,5,7,11,101,131,151,181,191]
-}

-- primosGemeos --
primosGemeos :: [(Int, Int)]
primosGemeos = [(y,x) | x <- [2 .. ], let y = x - 2, isPrime x, isPrime y]

{-
    primosGemeos retorna uma lista infinita de tuplas (y,x) onde y e x são primos e y = x - 2
    Verifico se y e x são primos com a função isPrime e se forem adiciono a tupla x e y
    Então take 10 primosGemeos -> [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
-}
