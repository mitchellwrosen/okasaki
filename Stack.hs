module Stack where

import Prelude hiding ((++), head, tail)

class Stack t where
    empty   :: t a
    isEmpty :: t a -> Bool
    cons    :: a -> t a -> t a
    head    :: t a -> a
    tail    :: t a -> t a

-- In zs == xs ++ ys, uses O(n) extra space, where n = length xs.
(++) :: Stack t => t a -> t a -> t a
xs ++ ys | isEmpty xs = ys
         | otherwise  = cons (head xs) (tail xs ++ ys)

-- O(n) time, O(n) space.
suffixes :: Stack t => t a -> t (t a)
suffixes xs | isEmpty xs = cons xs empty
            | otherwise  = cons xs (suffixes (tail xs))
