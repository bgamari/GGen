{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Data.VectorSpace.QuickCheck where

import Data.VectorSpace
import Test.QuickCheck
import Control.Monad (liftM)
import Foreign.Storable
import System.Random (Random)

-- | Normalized vector
newtype NormalizedV a = NormalizedV a
                        deriving (Show, Eq)

instance (s ~ Scalar v, Floating s, InnerSpace v, Arbitrary v) => Arbitrary (NormalizedV v) where
        arbitrary = do a <- arbitrary
                       return $ NormalizedV $ normalized a

-- | Number in [0,1]
newtype Normalized a = Normalized a
                       deriving (Show, Eq)

instance (Random a, Num a) => Arbitrary (Normalized a) where
        arbitrary = choose (0,1) >>= (return . Normalized)

newtype Parallel a = Parallel (a, a)
                     deriving (Show, Eq)

instance (s ~ Scalar v, Floating s, InnerSpace v, Arbitrary v) => Arbitrary (Parallel v) where
        arbitrary = do a <- arbitrary
                       b <- arbitrary
                       return $ Parallel (a, project b a)

