import Data.Map()
-- insertArvore -- 
{- data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)
insertArvore :: Ord a => a -> Arv a -> Arv a
insertArvore x Vazia = No x Vazia Vazia 
insertArvore x (No y esq dir)
    | x == y = No y esq dir
    | x < y = No y (insertArvore x esq) dir
    | otherwise = No y esq (insertArvore x dir) -}

-- foldTree -- 
{- data Arvore a = Folha a | Ramo (Arvore a) (Arvore a) deriving (Show)
foldTree :: (a->b) -> (b->b->b) -> Tree a -> b
foldTree  -}

-- makeMultiSet -- 
{- data MultiSet a = MultiSet [(a,Int)] deriving (Show, Eq)
--makeMultiSet :: [a] -> MultiSet
makeMultiSet (x:xs) =  -}

-- removeFolhas -- 
data ArvBin a = Vazia | No a ( ArvBin a ) ( ArvBin a ) deriving (Show, Eq)
removeFolhas :: Eq a => ArvBin a -> ArvBin a
removeFolhas Vazia = Vazia
removeFolhas (No x esq dir) 
    | esq /= Vazia && dir /= Vazia = No x (removeFolhas esq) (removeFolhas dir)
    | esq /= Vazia && dir == Vazia = No x (removeFolhas esq) Vazia
    | esq == Vazia && dir /= Vazia = No x Vazia (removeFolhas dir)
    | otherwise = Vazia

-- cheia -- 
cheia :: Eq a => ArvBin a -> Bool
cheia (No x esq dir)
    | esq /= Vazia && dir /= Vazia = cheia esq && cheia dir
    | esq == Vazia && dir == Vazia = True
    | otherwise = False