import qualified Data.Map as Map
import Data.List

-- Frequency of a given character in a string
data CharFreq = CharFreq {
    value :: Char,
    frequency :: Int
} deriving (Show)

instance Eq CharFreq where 
    (CharFreq _ freq1) == (CharFreq _ freq2) = freq1 == freq2

instance Ord CharFreq where
    (CharFreq _ freq1) < (CharFreq _ freq2) = freq1 < freq2
    (CharFreq _ freq1) <= (CharFreq _ freq2) = freq1 <= freq2
    (CharFreq _ freq1) > (CharFreq _ freq2) = freq1 > freq2
    (CharFreq _ freq1) >= (CharFreq _ freq2) = freq1 >= freq2

-- A tree consists of node, left child tree, right child tree
data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
    deriving (Show)

getFreq :: String -> [CharFreq]
getFreq l@(x:xs) = 
    quicksort $ zipWith CharFreq uniques (map (\x -> freq x l) uniques)
    where 
        uniques = findUniques l
        freq :: Char -> String -> Int
        freq x (y:ys)
            | x == y    = 1 + freq x ys
            | otherwise = freq x ys
        freq x [] = 0

findUniques :: String -> [Char]
findUniques [] = []
findUniques (x:xs) = x : findUniques [ y | y <- xs, y /= x ]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort l@(x:xs) =
    let lesser = quicksort $ filter (< x) xs
        greater = quicksort $ filter (>= x) xs
    in lesser ++ [x] ++ greater

-- get the CharFreq value from a Node or Leaf
getCF x =
    case x of
        (Node v l r)    -> v
        (Leaf v)        -> v

instance Eq a => Eq (Tree a) where 
    a == b = getCF a == getCF b

instance Ord a => Ord (Tree a) where
    a < b = getCF a < getCF b
    a <= b = getCF a <= getCF b
    a > b = getCF a > getCF b
    a >= b = getCF a >= getCF b

-- Initial forest is all leaves

-- generate the initial forest
genLeaves :: [CharFreq] -> [Tree CharFreq]
genLeaves (x:xs) = Leaf x : genLeaves xs
genLeaves [] = []

-- create the huffman tree
--createHuffman [] = []
--createHuffman (x:y:ys) 

