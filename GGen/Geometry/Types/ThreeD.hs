{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
           , TypeFamilies
           , ViewPatterns
           , MultiParamTypeClasses
           , GeneralizedNewtypeDeriving
           , DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Types
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for three-dimensional Euclidean space.
--
-----------------------------------------------------------------------------

module GGen.Geometry.Types.ThreeD
       ( -- * 3D Euclidean space
         R3, r3, unr3
       , P3, p3, unp3
       , T3

       ) where

import Graphics.Rendering.Diagrams

import Control.Newtype

import Data.Typeable       
import Data.Basis
import Data.Cross
import Data.VectorSpace
import Data.NumInstances ()

------------------------------------------------------------
-- 3D Euclidean space

-- | The three-dimensional Euclidean vector space R^3.
newtype R3 = R3 { unR3 :: (Double, Double, Double) }
  deriving (AdditiveGroup, Eq, Ord, Show, Read, Typeable, Num, Fractional, HasCross3)

instance Newtype R3 (Double, Double, Double) where
  pack   = R3
  unpack = unR3

-- | Construct a 3D vector from a triple of components.
r3 :: (Double, Double, Double) -> R3
r3 = pack

-- | Convert a 3D vector back into a triple of components.
unr3 :: R3 -> (Double, Double, Double)
unr3 = unpack

type instance V R3 = R3

instance VectorSpace R3 where
  type Scalar R3 = Double
  (*^) = over R3 . (*^)

instance HasBasis R3 where
  type Basis R3 = Either () (Either () ()) -- = Basis (Double, Double, Double)
  basisValue = R3 . basisValue
  decompose  = decompose  . unR3
  decompose' = decompose' . unR3

instance InnerSpace R3 where
  (unR3 -> vec1) <.> (unR3 -> vec2) = vec1 <.> vec2

-- | Points in R^3.
type P3 = Point R3

-- | Construct a 3D point from a triple of coordinates.
p3 :: (Double, Double, Double) -> P3
p3 = pack . pack

-- | Convert a 2D point back into a triple of coordinates.
unp3 :: P3 -> (Double, Double, Double)
unp3 = unpack . unpack

-- | Transformations in R^3.
type T3 = Transformation R3

instance Transformable R3 where
  transform = apply
