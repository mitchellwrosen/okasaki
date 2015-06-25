{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module FiniteMap where

import Control.Exception
import GHC.Exts          (Constraint)

data NotFound = NotFound
  deriving Show

instance Exception NotFound

class FiniteMap t where
    type C t :: Constraint
    type C t = ()

    type Key t :: *
    type Val t :: *

    empty  :: t
    bind   :: Key t -> Val t -> t -> t
    lookup :: C t => Key t -> t -> Val t
