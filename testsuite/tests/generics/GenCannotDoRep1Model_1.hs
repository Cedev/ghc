{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module CannotDoRep1Model_2 where

import GHC.Generics (Generic, Generic1)


-- We cannot generate a useful generic representation for this strange composition

data Strange f = Strange (f (f Int))                   deriving Generic1
