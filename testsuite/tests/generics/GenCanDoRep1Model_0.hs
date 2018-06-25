{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module CanDoRep1Model_0 where

import GHC.Generics (Generic, Generic1)


-- We should be able to generate a generic representation for these models

data Parent f = Parent {
    name :: f String,
    child :: f (Child f)
}
  deriving (Generic1)
  
data Child f = Child {
    ordinal :: Int,
    nickname :: f String
}
  deriving (Generic1)
  
data Fix f = In (f (Fix f))                                 deriving Generic1

data ListF a f = NilF | ConsF (f a) (f (ListF a f))         deriving Generic1

data RoseF a f = RoseF a [f (RoseF a f)]                    deriving Generic1

data ParAp0 c p = ParAp0 (p c)                              deriving Generic1
