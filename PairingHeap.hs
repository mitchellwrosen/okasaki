{-# LANGUAGE PatternSynonyms #-}

module PairingHeap where

import Heap

data PHeapT a
    = PT a [PHeapT a]

newtype PHeap a = P (Maybe (PHeapT a))

pattern E = P Nothing
pattern T a as = P (Just (PT a as))

instance Heap PHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T x []) h

    merge h E = h
    merge E h = h
    merge (T x hs1) (T y hs2)
        | x <= y    = T x (PT y hs2 : hs1)
        | otherwise = T y (PT x hs1 : hs2)

    findMin E = Nothing
    findMin (T x _) = Just x

    deleteMin E = E
    deleteMin (T _ hs) = mergePairs hs
      where
        mergePairs []  = E
        mergePairs [h] = mkHeap h
        mergePairs (h1:h2:hs) = merge (merge (mkHeap h1) (mkHeap h2)) (mergePairs hs)

        mkHeap = P . Just
