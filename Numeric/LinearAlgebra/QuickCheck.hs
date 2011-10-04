module Numeric.LinearAlgebra.QuickCheck where

import Numeric.LinearAlgebra
import Test.QuickCheck
import Control.Monad (liftM)
import Foreign.Storable

instance (Arbitrary a, Storable a, RealFrac a) => Arbitrary (Vector a) where
        arbitrary = (liftM $ fromList . take 3) $ vector 3

