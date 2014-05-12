module Es4 where

-- Length
myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength2 :: [a] -> Int
myLength2 [] = 0
myLength2 (_:xs) = 1 + myLength2 xs

-- Range
myRange :: Int -> Int -> [Int]
myRange a b = if a > b
                                then error "Low > High"
                                else if a < b
                                                then a : myRange (a + 1) b
                                                else [a]

myRange2 :: Int -> Int -> [Int]
myRange2 a b
        | a > b = error "Low > High"
        | a == b = [a]
        | a < b = a : myRange2 (a + 1) b

myRange3 :: Int -> [Int]
myRange3 a = a : myRange3 (a + 1)

-- Map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Fibonacci
myFib :: Int -> Int
myFib a
        | a == 0 = 0
        | a == 1 = 1
        | otherwise = myFib (a - 1) + myFib (a - 2)

myFib2 :: [Int]
myFib2 = 1 : 2 : zipWith (+) myFib2 (tail myFib2)

-- Takewhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x : xs)
        | f x = x : myTakeWhile f xs
        | otherwise = []

-- Pack
packHelper :: Eq a => [a] -> [[a]] -> [a] -> [[a]]
packHelper [] acc sub = sub:acc
packHelper (x:xs) acc [] = packHelper xs acc [x]
packHelper (x:xs) acc (y:ys)
        | x == y = packHelper xs acc (x:sub)
        | otherwise = packHelper xs (sub:acc) [x]
        where sub = y:ys

pack :: Eq a => [a] -> [[a]]
pack input = reverse (packHelper input [] [])

-- Encode
encode :: Eq a => [a] -> [(a, Int)]
encode input = zip (map head packed) (map length packed)
        where packed = pack input

-- K combinations
combination 0 _ = [[]]
combination _ [] = []
combination n (x:xs) = (map (\y -> x : y) (combination (n - 1) xs)) ++ (combination n xs)

-- Binary trees
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
        | x == a = Node x left right
        | x < a = Node a (treeInsert x left) right
        | x > a = Node a left (treeInsert x right)

treeSum :: Num a => Tree a -> a
treeSum EmptyTree = 0
treeSum (Node a left right) = a + (treeSum left) + (treeSum right)

treeValues EmptyTree = []
treeValues (Node a left right) = a : ((treeValues left) ++ (treeValues right))
