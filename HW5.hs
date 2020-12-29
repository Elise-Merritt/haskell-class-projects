-- Homework 5
-- Elise Merritt

module HW5 where

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)
data Rose a = Node a [Rose a] deriving (Show, Eq)

fromTree :: Tree a -> [a]
fromTree Tip = []
fromTree (Bin l x r) = fromTree l ++ [x] ++ fromTree r

trunc :: Int -> Tree a -> Tree a
trunc i Tip = Tip
trunc i (Bin l x r) = if i<1 then Tip else (Bin (trunc (i-1) l) x (trunc (i-1) r))

symmetric :: (Eq a) => Tree a -> Bool
symmetric Tip = True
symmetric (Bin Tip x Tip) = True
symmetric (Bin (Bin l1 x1 r1) a (Bin l2 x2 r2)) = if x1==x2 && l1==r2 && r1==l2 then True else False

sumRose :: (Num a) => Rose a -> a
sumRose (Node x [])=x
sumRose (Node x y) = x + sum (map sumRose y)

maximumRose :: (Ord a) => Rose a -> a
maximumRose (Node x []) = x
maximumRose (Node x y) = maximum ([x] ++ [maximum (map maximumRose y)])

sizeRose :: Rose a -> Int
sizeRose (Node x []) = 1
sizeRose (Node x [Node y z]) = 1 + (sizeRose (Node y z))
sizeRose (Node x y) = 1 + length (map sizeRose y)

fanout :: Rose a -> Int
fanout (Node x []) = 0
fanout (Node x [Node y z]) = 1 + (fanout (Node y z))
fanout (Node x y) = length (map fanout y)

toRoses :: Tree a -> [Rose a]
toRoses Tip = []
toRoses (Bin Tip x Tip) = [Node x []]
toRoses (Bin l x Tip) = [Node x (toRoses l)]
toRoses (Bin Tip x r) = [Node x (toRoses r)]
toRoses (Bin l x r) = [Node x ((toRoses l) ++ (toRoses r))]

fromRoses :: [Rose a] -> Tree a
fromRoses [] = Tip
fromRoses [Node x []] = (Bin Tip x Tip)
fromRoses [Node x [Node y z]] = (Bin Tip x (fromRoses [Node y z]))
fromRoses [Node x y] = (Bin Tip x (fromRoses y))

