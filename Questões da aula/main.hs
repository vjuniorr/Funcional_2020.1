-- Aula 20/08 codigos do site sobre tipo --

{- 
data Bool = False | True
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  
-}

data Point = Point{x :: Int, y :: Int} deriving (Show)
data Shape = Circle {center :: Point, radius :: Float} | Rectangle {pos :: Point, dim :: Point} deriving (Show)
 

{- surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  -}

{- 
inside :: Point -> Shape -> Bool
inside point (Circle center radius) 
    | sqrt ((x1 - x2)^2 + (y1 - y2)^2) > radius = False
    | otherwise = True; -}


{- Aula 27/08 -}


rot xs n = drop n xs ++ take n xs 

{- Aula 03/09 -}
buscaPos' :: Int -> [Int]-> Int -> Int
buscaPos :: Int -> [Int] -> Int -- valor, vet, primeira posição que ele aparece
buscaPos' v [] i = -1 
buscaPos' v (x:xs) i = if v == x then i else buscaPos' v xs (i + 1)

buscaPos valor vet = 
    buscaPos' valor vet 0


type AssocList k v = [(k, v)]


-- [(3, "uva"), (5, banana) , (2, chiclete)] --

--getValue :: Int -> AssocList Int String -> Maybe String
--getValue key mapa = if null posicoes then Nothing else Just $ head posicoes
--    where posicoes [v | (k, v) <- mapa, k == key]

data Calc = Calc {battery :: Int} deriving (Show)

divide :: Calc -> Int -> Int -> Either String Int
divide (Calc bat) x y 
    | bat == 0 = Left "Bateria vazia"
    | y == 0 = Left "nao eh possivel dividir por 0"
    | otherwise = Right x / y
