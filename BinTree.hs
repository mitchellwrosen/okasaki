-- Binomial tree

module BinTree where

import Heap

data BinTree a = Node Int a [BinTree a]

rank :: BinTree a -> Int
rank (Node r _ _) = r

root :: BinTree a -> a
root (Node _ x _) = x

-- | Trees must have equal rank.
link :: Ord a => BinTree a -> BinTree a -> BinTree a
link t1@(Node r x1 c1) t2@(Node s x2 c2)
    | r /= s    = error "cannot link trees of different rank"
    | x1 <= x2  = Node (r+1) x1 (t2:c1)
    | otherwise = Node (r+1) x2 (t1:c2)

data BinHeap a
    = E
    | T (BinTree a) (BinHeap a)

insTree :: Ord a => BinTree a -> BinHeap a -> BinHeap a
insTree t E = T t E
insTree t ts@(T t' ts')
    | rank t < rank t' = T t ts
    | otherwise        = insTree (link t t') ts'

mkHeap :: [BinTree a] -> BinHeap a
mkHeap [] = E
mkHeap (t:ts) = T t (mkHeap ts)

instance Heap BinHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x ts = insTree (Node 0 x []) ts

    merge ts1 E = ts1
    merge E ts2 = ts2
    merge ts1@(T t1 ts1') ts2@(T t2 ts2')
        | rank t1 < rank t2 = T t1 (merge ts1' ts2)
        | rank t2 < rank t1 = T t2 (merge ts1 ts2')
        | otherwise         = insTree (link t1 t2) (merge ts1' ts2')

    findMin E = Nothing
    findMin h = Just . root . fst . removeMinTree $ h

    deleteMin E = E
    deleteMin ts =
        let (Node _ x ts1, ts2) = removeMinTree ts
        in merge (mkHeap (reverse ts1)) ts2

removeMinTree :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMinTree E = error "removeMinTree: E"
removeMinTree (T t E) = (t, E)
removeMinTree (T t ts) =
    let (t', ts') = removeMinTree ts
    in if root t < root t'
           then (t, ts)
           else (t', T t ts')
