module SplayHeap where

import Heap

data SHeap a
    = E
    | T (SHeap a) a (SHeap a)

instance Heap SHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x t = T (smaller x t) x (bigger x t)

    merge E t = t
    merge t E = t
    merge (T a x b) t = T (merge (smaller x t) a) x (merge (bigger x t) b)

    findMin E = Nothing
    findMin (T E x _) = Just x
    findMin (T a _ _) = findMin a

    deleteMin E = E
    deleteMin (T E _ b) = b
    deleteMin (T (T E x b) y c) = T b y c
    --     y          x
    --    / \        / \
    --   x   c  =>  a'  y
    --  / \            / \
    -- a   b          b   c
    deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)


smaller :: Ord a => a -> SHeap a -> SHeap a
smaller _ E = E
smaller pivot (T a x b)
    | pivot >= x =
        case b of
            E -> T a x E
            T a1 y a2
                --    x                    y
                --  /   \                /   \
                -- a      y      =>     x     a2'
                --      /   \         /   \
                --    a1     a2      a     a1
                | pivot >= y -> T (T a x a1) y (smaller pivot a2)
                | otherwise  -> T a x (smaller pivot a1)

bigger :: Ord a => a -> SHeap a -> SHeap a
bigger _ E = E
bigger pivot (T a x b)
    | pivot > x = bigger pivot b
    | otherwise =
        case a of
            E -> T E x b
            T a1 y a2
                | pivot > y -> T (bigger pivot a2) x b
                --        x              y
                --      /   \          /   \
                --     y      b  =>  a1'     x
                --   /   \                 /   \
                -- a1     a2             a2      b
                | otherwise -> T (bigger pivot a1) y (T a2 x b)
