import Queue ()
import qualified Data.Set as Set 
import Data.Char ( isLetter )
import Data.Char( toLower)

{- -- josephus --
josephus :: Int -> Int -> Int
josephus n k =  


pula :: Int -> Queue a -> Queue a
pula n x = makeQueue ( drop n (makeList x) ++ take n (makeList x))

makeList :: Queue a -> [a]
makeList x 
    | isEmpty x = []
    | otherwise = front x : makeList (dequeue x)

simula :: Int -> Queue a -> Int
simula k q = 
 -}

-- isograma -- 
isograma :: String -> Bool
isograma str = 
    let straux = aux str
    in if(Set.size (Set.filter (/= '-')(Set.fromList str)) == length(straux)) then True else False

aux :: String -> String
aux "" = ""
aux (s:str) 
    | s /= '-' = s : aux str
    | otherwise = aux str


-- Pangrama -- 
isPangram :: String -> Bool 
isPangram str 
    | length (map toLower (Set.toList $ Set.filter (isLetter) $ Set.fromList str)) == length ['a' .. 'z'] = True
    | otherwise = False
{- 
-- numeroTrocas -- 
--numeroTrocas :: [Int] -> [Int] -> Int
numeroTrocas xs ys = length (repetidos xs) + length (repetidos ys)

repetidos [] = []
repetidos (x:xs)
    | x `elem` xs = x : repetidos xs
    | otherwise = repetidos xs

 -}

{- numeroInsercao :: String -> Int
numeroInsercao str
    | str == reverse str = 0
    |  -}