module GGen.Types ( Extrude(..)
                  , ToolMove(..)
                  , ToolPath
                  ) where

import GGen.Geometry.Types

-- | Specifies whether the extruder should extrude during a move
data Extrude = Extrude Double
             | Dry
             deriving (Show)

-- | Specifies a tool motion
-- Since we build in layers, we break tool moves into two classes:
-- moves within a slice (XYMove) and moves between slices (ZMove)
data ToolMove = XYMove Point2 Extrude
              | ZMove Double
              deriving (Show)

type ToolPath = [ToolMove]

