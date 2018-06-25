{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
module T10604_bad_variable_occurrence where

import GHC.Generics

newtype Twice f = Twice ((f Int) Int)
  deriving Generic1
