module Es6 where

import Control.Monad

-- Monads
-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--     fail :: String -> m a
--     fail msg = error msg

-- Logger monad
type Log = [String]
newtype Logger a = Logger { execLogger :: (a, Log) }

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m
                  n      = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

instance Show a => Show (Logger a) where
    show (Logger a) = show a

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

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

treeSum :: Num a => Tree a -> a
treeSum EmptyTree = 0
treeSum (Node a left right) = a + (treeSum left) + (treeSum right)

treeFold :: (a -> b -> a) -> a -> Tree b -> a
treeFold f acc EmptyTree = acc
treeFold f acc (Node b left right) = treeFold f (f (treeFold f acc left) b) right

singletonM :: (Show a) => a -> Logger (Tree a)
singletonM x = do
    record ("Created singleton " ++ show x)
    return (Node x EmptyTree EmptyTree)

-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldM treeInsertM EmptyTree nums
-- (foldM because treeInsertM returns a monad instead of the standard data structure)
treeInsertM :: (Ord a, Show a) => Tree a -> a -> Logger (Tree a)
treeInsertM EmptyTree x = singletonM x
treeInsertM (Node a left right) x
    | x == a = do
        record ("Inserted " ++ show x)
        return (Node x left right)
    | x < a = do
        l <- treeInsertM left x
        return (Node a l right)
    | x > a = do
        r <- treeInsertM right x
        return (Node a left r)

-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = do
--   x <- m
--   return (f x)
treeSumM :: Num a => Logger (Tree a) -> Logger a
treeSumM t = liftM (treeFold (+) 0) t

-- filterM treeBalanced [t1, t2]
andM :: Logger Bool -> Logger Bool -> Logger Bool
andM l1 l2 = do
    c1 <- l1
    c2 <- l2
    return (c1 && c2)

treeBalancedM :: Tree a -> Logger Bool
treeBalancedM EmptyTree = do
    record "An empty tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree EmptyTree) = do
    record "A single node tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree _) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ _ EmptyTree) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ left right) = andM (treeBalancedM left) (treeBalancedM right)

-- foldM
treeSumM2 :: (Num a, Ord a) => Tree a -> Logger a
treeSumM2 t = do
    if s > 10
       then record "Big tree"
       else record "Small tree"
    return s
    where s = treeSum t
