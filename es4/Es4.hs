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

-- WARNING: infinite list, use "take"!
myRange3 :: Int -> [Int]
myRange3 a = a : myRange3 (a + 1)

-- Map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- List comprehensions
rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]

-- WARNING: infinite list, use "take"!
rightTriangles2 :: [(Integer, Integer, Integer)]
rightTriangles2 = [(a,b,c) | c <- [1,2..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 

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

-- Binary trees
-- let empty = EmptyTree
-- let singleNode = Node 10 EmptyTree EmptyTree
-- let twoNodes = Node 5 empty singleNode
-- ...
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Implementing a type class
-- 
-- Definition from the Prelude:
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)
--
-- This means that a) we have to implement both "equals" and "not equals"
-- and b) since "x is equal to y if x is not equal to y" and viceversa,
-- we can just define "equals" or "not equals" and Haskell will infer the
-- other one.

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
