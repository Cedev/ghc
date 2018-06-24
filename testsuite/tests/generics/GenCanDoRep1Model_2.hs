{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module CanDoRep1Model where

import GHC.Generics (Generic, Generic1)


-- We should be able to generate a generic representation for these difficult compositions

data D f = D                                           deriving Generic1

data Compose2 f = Comp2 (f [D f])                      deriving Generic1

data Compose3 f = Comp3 (f [[D f]])                    deriving Generic1

data Compose10 f = Comp10 (f [[[[[[[[[D f]]]]]]]]])    deriving Generic1
