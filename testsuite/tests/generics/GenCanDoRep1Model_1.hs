{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module CanDoRep1Model_1 where

import GHC.Generics (Generic, Generic1)


-- We should be able to generate a generic representation for ParAp1

data ParAp1 f p = ParAp1 (p (f p))                     deriving Generic1
