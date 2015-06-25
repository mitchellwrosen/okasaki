{-# LANGUAGE InstanceSigs #-}

module List where

import Stack

import Prelude hiding (head, tail)

data List a
    = Nil
    | Cons !a (List a)

instance Stack List where
    empty :: List a
    empty = Nil

    isEmpty :: List a -> Bool
    isEmpty Nil = True
    isEmpty _   = False

    cons :: a -> List a -> List a
    cons = Cons

    head :: List a -> a
    head (Cons x _) = x

    tail :: List a -> List a
    tail (Cons _ xs) = xs
