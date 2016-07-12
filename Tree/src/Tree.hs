module Tree where

import  Data.List
import System.IO      -- for hFlush


data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

main :: IO ()
main = do
     return ()

single :: a -> Tree a
single a  = Node Leaf a Leaf

size :: Tree a -> Int
size  Leaf  = 0
size  (Node t1 x t2) = 1 + size t1 + size t2

flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node t1 x t2) = (flatten t1) ++ x:[] ++ flatten t2

reverse' :: Tree a -> Tree a
reverse' (Node Leaf x Leaf) = Node Leaf x Leaf
reverse' (Node t1 x t2) = Node (reverse' t2) x (reverse' t1)

treeSort :: Ord x => [x] -> Tree x
treeSort xs = treeSort' xs Leaf

treeSort' :: Ord x => [x] -> Tree x -> Tree x
treeSort' [] t = t
treeSort' (x:xs) t = treeSort' xs (insert x t)

insert :: Ord x => x -> Tree x -> Tree x
insert x Leaf = Node Leaf x Leaf
insert y (Node t1 x t2) = if y >= x  then (Node t1 x (insert y t2)) else (Node (insert y t1) x t2)
