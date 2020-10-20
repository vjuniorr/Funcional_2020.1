import Data.List()
-- Linked List -- 
data LinkedList a = Vazia | No a (LinkedList a) deriving (Eq, Show)

fromList :: [a] -> LinkedList a
fromList [] = Vazia
fromList xs = foldr No Vazia xs

toList :: LinkedList a -> [a]
toList Vazia = []
toList (x `No` xs) = x : toList xs

append :: a -> LinkedList a -> LinkedList a
append x l =  fromList (toList l ++ [x])

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList xs = fromList (reverse (toList xs))

-- Mobile -- 
data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)

--balanceado :: Mobile -> Bool
balanceado :: Mobile -> Bool
balanceado (Barra mob1 mob2) 
    | peso mob1 == peso mob2 = True
    | otherwise = False
--peso :: Mobile -> Int
peso :: Mobile -> Int
peso (Pendente p1) = p1

-- eqsplits -- 
{- eqsplits :: [a] -> [([a],[a])]
eqsplits xs =  -} 

-- splits -- 
--splits :: [a] -> [([a],[a])]
{- splits xs = splitsaux (length xs) xs 
splitsaux n xs 
    | n /= 0 = [splitAt n xs] ++ splitsaux (n-1) xs
    | otherwise = [] -}

-- numPassageiros -- 
data Trem a = Vagao a ( Trem a ) | Vazio deriving Show
type Quantidade = Int
type Peso = Int
data Carga = SemCarga | Passageiro Quantidade | Mercadoria Peso deriving Show

{- numPassageiros :: Trem Carga -> Int
numPassageiros (x `Vagao` xs) ys -}
