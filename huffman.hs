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

{-  Create the huffman tree.
    The given input should a List of Leaves, and should be sorted already.
    Therefore, x and y should be the Leaves with the least occurin chars.
-}
createHuffman :: [Tree CharFreq] -> [Tree CharFreq]
createHuffman (x:y:ys) = createHuffman nList
    where 
        nList 
            | length ys > 0    = quicksort $ (merge x y) : ys
            | otherwise = [merge x y]
-- single item or empty tree
createHuffman x = x

-- merge two trees into one
merge :: Tree CharFreq -> Tree CharFreq -> Tree CharFreq
merge t1 t2 
    | t1 <= t2      = Node (CharFreq '*' newWeight) t1 t2
    | otherwise     = Node (CharFreq '*' newWeight) t2 t1
        where newWeight = ((frequency $ getCF t1) + (frequency $ getCF t2))

-- generate an Huffman encoding for the given CharFreq
genEncoding :: CharFreq -> Tree CharFreq -> [String]
genEncoding x (Node n l r) = huffEither ("0" : genEncoding x l) ("1" : genEncoding x r)
genEncoding x (Leaf n)
    | value x == value n = [""]
    | otherwise          = ["-1"]

-- if ends in -1, is not correct target
huffEither a b
    | last a == "-1"    = b
    | otherwise         = a

getAllEncoding :: [CharFreq] -> Tree CharFreq -> [(Char, [Char])]
getAllEncoding list tree = zip (map value list) (map (\x -> concat $ genEncoding x tree) list)

decode :: [Char] -> Tree CharFreq -> Maybe Char
decode (x:xs) (Node n l r) 
    | x == '0'  = decode xs l
    | x == '1'  = decode xs r
    | otherwise = Nothing
decode [] (Leaf x) = Just $ value x
decode _ (Leaf x) = Nothing

