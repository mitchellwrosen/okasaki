module RBTree where

data Color = R | B

data RBTree a
    = E -- Black
    | T Color (RBTree a) a (RBTree a)

member :: Ord a => a -> RBTree a -> Bool
member x E = False
member x (T _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

insert :: Ord a => a -> RBTree a -> RBTree a
insert x s =
    let T _ a y b = ins s
    in T B a y b
  where
    ins E = T R E x E
    ins s@(T c a y b)
        | x < y     = balance c (ins a) y b
        | x > y     = balance c a y (ins b)
        | otherwise = s

    -- Balance B-R-R (4 permutations) to R-B-R (1 permutation)
    balance B (T R (T R a x b) y (c        )) z (d                            ) = T R (T B a x b) y (T B c z d)
    balance B (T R (a        ) x (T R b y c)) z (d                            ) = T R (T B a x b) y (T B c z d)
    balance B (a                            ) x (T R (T R b y c) z (d        )) = T R (T B a x b) y (T B c z d)
    balance B (a                            ) x (T R (b        ) y (T R c z d)) = T R (T B a x b) y (T B c z d)
    balance c a y b                                                             = T c a y b
