module LeftistHeap where

import Heap

data LHeap a
    = E
    | T Int a (LHeap a) (LHeap a)
    deriving (Eq, Ord)

rank :: LHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

instance Heap LHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    merge h E = h
    merge E h = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
        | x <= y    = makeT x a1 (merge b1 h2)
        | otherwise = makeT y a2 (merge h1 b2)
      where
        makeT :: a -> LHeap a -> LHeap a -> LHeap a
        makeT x a b
            | rank a >= rank b = T (rank b + 1) x a b
            | otherwise        = T (rank a + 1) x b a

    insert x h = merge (T 1 x E E) h

    findMin E = Nothing
    findMin (T _ x _ _) = Just x

    deleteMin E = E
    deleteMin (T _ _ a b) = merge a b
