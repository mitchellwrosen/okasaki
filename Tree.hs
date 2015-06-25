{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Tree where

import FiniteMap
import Set

import Control.Exception (throw)
import Prelude           hiding (lookup)

data Tree a
    = E
    | T (Tree a) !a (Tree a)
    deriving Show

instance Ord a => Set (Tree a) where
    type Elem (Tree a) = a

    empty :: Tree a
    empty = E

    insert :: a -> Tree a -> Tree a
    insert x E = T E x E
    insert x s@(T a y b)
        | x < y     = T (insert x a) y b
        | x > y     = T a y (insert x b)
        | otherwise = s

    member :: a -> Tree a -> Bool
    member _ E = False
    member x (T a y b)
        | x < y     = member x a
        | x > y     = member x b
        | otherwise = True

instance Ord k => FiniteMap (Tree (k, v)) where
    type C   (Tree (k, v)) = Ord k
    type Key (Tree (k, v)) = k
    type Val (Tree (k, v)) = v

    empty :: Tree (k, v)
    empty = E

    bind :: k -> v -> Tree (k, v) -> Tree (k, v)
    bind k v E = T E (k,v) E
    bind k v (T a kv@(k',v') b) =
        case compare k k' of
            LT -> T (bind k v a) kv b
            GT -> T a kv (bind k v b)
            EQ -> T a (k,v) b

    lookup :: Ord k => k -> Tree (k,v) -> v
    lookup _ E = throw NotFound
    lookup k (T a (k',v) b) =
        case compare k k' of
            LT -> lookup k a
            GT -> lookup k b
            EQ -> v

-- Exercise 2.2
--
-- member performs approximately 2d comparisons, where d is the depth of the tree.
-- member' performs no more than d+1 comparisons.
member' :: forall a. Ord a => a -> Tree a -> Bool
member' _ E = False
member' x (T a y b)
    | x < y     = member x a
    | otherwise = go y b
  where
    go :: a -> Tree a -> Bool
    go m E = x == m
    go m (T a y b)
        | x < y     = go m a
        | otherwise = go y b

-- Exercise 2.5a
--
-- complete x d creates a complete binary tree of depth d with x stored in
-- every node. O(n).
complete :: a -> Int -> Tree a
complete _ 0 = E
complete x n = let t = complete x (n-1)
               in T t x t

-- Exercise 2.5b
--
-- complete' x d creates a balanced tree of arbitrary size (siblings size varies
-- by at most one). O(log n).
complete' :: a -> Int -> Tree a
complete' x 0 = E
complete' x n = let (a,b) = (n-1) `divMod` 2
                in T (complete' x a) x (complete' x (a+b))
