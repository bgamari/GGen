{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Data.VectorSpace.QuickCheck where

import Data.VectorSpace
import Test.QuickCheck
import Control.Monad (liftM)
import Foreign.Storable

newtype Normalized a = Normalized a

instance (s ~ Scalar v, Floating s, InnerSpace v, Arbitrary v) => Arbitrary (Normalized v) where
        arbitrary = do a <- arbitrary
                       return $ Normalized $ normalized a

newtype Parallel a = Parallel (a, a)

instance (s ~ Scalar v, Floating s, InnerSpace v, Arbitrary v) => Arbitrary (Parallel v) where
        arbitrary = do a <- arbitrary
                       b <- arbitrary
                       return $ Parallel (a, project b a)

