module Heap where

class Heap t where
    empty   :: t a
    isEmpty :: t a -> Bool

    insert :: Ord a => a -> t a -> t a
    merge :: Ord a => t a -> t a -> t a

    findMin :: Ord a => t a -> Maybe a
    deleteMin :: Ord a => t a -> t a
