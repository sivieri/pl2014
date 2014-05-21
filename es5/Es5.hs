module Es5 where

import qualified Data.Map as M
import Control.Monad

-- Folds
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

-- Binary trees
-- let nums = [8,6,4,1,7,3,5]
-- let t = foldr treeInsert EmptyTree nums
-- (foldr because treeInsert takes the current tree as second parameter)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

treeSum :: Num a => Tree a -> a
treeSum EmptyTree = 0
treeSum (Node a left right) = a + (treeSum left) + (treeSum right)

treeValues :: Tree a -> [a]
treeValues EmptyTree = []
treeValues (Node a left right) = a : ((treeValues left) ++ (treeValues right))

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f EmptyTree = EmptyTree
treeMap f (Node a left right) = Node (f a ) (treeMap f left) (treeMap f right)

treeFold :: (a -> b -> a) -> a -> Tree b -> a
treeFold f acc EmptyTree = acc
treeFold f acc (Node b left right) = treeFold f (f (treeFold f acc left) b) right

treeValues2 :: Tree a -> [a]
treeValues2 t = treeFold (++) [] (treeMap (:[]) t)

-- You can think the typeclass Functor as simply a typeclass used to be able to apply
-- a function to each element of your structure. Basically, any data structure that
-- works as a container for some data, can be a Functor
-- (cfr. the books for the theory that goes behind the concept of Functor)
instance Functor Tree where
        fmap f EmptyTree = EmptyTree
        fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

-- The same concept exists for fold, and it comes from the typeclass Foldable
-- (cfr. the books for more details).

-- Monads
-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--     fail :: String -> m a
--     fail msg = error msg

-- Maybe monad
-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= f = Nothing
--     Just x >>= f  = f x
--     fail _ = Nothing

-- k-v map monad
type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap

findCarrierBillingAddress2 :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress2 person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    M.lookup carrier addressMap
