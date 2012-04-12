module GGen.Types ( Exposure (..)
                  , Slice
                  , InfillRatio
                  , Angle
                  , module GGen.ToolPath.Types
                  ) where

import GGen.Geometry.Types
import GGen.ToolPath.Types       

-- | Is a polygon facing the outside world or internal to the object?
data Exposure = External | Internal deriving (Show, Eq)

-- | A slice is defined by a Z coordinate and a set of oriented polygons
type Slice = (Double, [(OrientedPolygon R2, Exposure)])

-- | An infill ratio
type InfillRatio = Double

-- | An angle in degrees
type Angle = Double

