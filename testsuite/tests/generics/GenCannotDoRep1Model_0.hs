{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module CannotDoRep1Model_0 where

import GHC.Generics (Generic, Generic1)


-- We are not yet able to generate a generic representation for this difficult composition

data D f = D                                           deriving Generic1

data Compose2 f = Comp2 (f [D f])                      deriving Generic1

-- Nor for these larger compositions

-- data Compose3 f = Comp3 (f [[D f]])                    deriving Generic1
-- data Compose10 f = Comp10 (f [[[[[[[[[D f]]]]]]]]])    deriving Generic1
