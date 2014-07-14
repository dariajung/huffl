import qualified Data.Map as Map
import Data.List

-- Frequency of a given character in a string
data CharFreq = CharFreq {
    value :: Char,
    frequency :: Int
} deriving (Show)

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