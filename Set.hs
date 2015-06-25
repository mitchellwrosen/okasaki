{-# LANGUAGE TypeFamilies #-}

module Set where

class Set t where
    type Elem t :: *

    empty  :: t
    insert :: Elem t -> t -> t
    member :: Elem t -> t -> Bool
