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

getFreq :: String -> [CharFreq]
getFreq l@(x:xs) = 
    zipWith CharFreq uniques (map (\x -> freq x l) uniques)
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