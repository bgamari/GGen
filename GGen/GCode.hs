module GGen.GCode ( slicesToGCode
                  , GCommand
                  ) where

import Text.Printf
import Data.VectorSpace

import GGen.Geometry.Types
import GGen.Types

type GCommand = String

-- | Diameter of filament
filamentDia = 3 -- millimeters

-- | Cross sectional area of filament
filamentArea = pi * (filamentDia/2)**2

-- | Diameter of extrudant
extrusionDia = 0.3 -- millimeters

-- | The ratio of actual feed distance to E axis distance (0 means completely
-- slipping, 1 means no slip)
eSlipRate = 1

-- | True volume extruded per mm E axis motion
eRate = filamentArea / eSlipRate


prelude = [ "# Begin prelude"
          , "G161 X0 Y0 Z0" 
          , "# End prelude"
          ]

postlude = [ "# Begin postlude"
          , "G1 Z10"
          , "G1 X0 Y0"
          , "# End postlude"
          ]

-- TODO: Retract filament
toolMoveToGCode :: ToolMove -> GCommand
toolMoveToGCode (ToolMove (LineSeg _ (x,y)) Dry) =
        printf "G1 X%1.2f Y%1.2f" x y
toolMoveToGCode (ToolMove l@(LineSeg _ (x,y)) (Extrude e)) =
        printf "G1 X%1.2f Y%1.2f E%1.2f" x y (e*magnitude (lsDispl l)/eRate)

sliceToGCode :: (Double, ToolPath) -> [GCommand]
sliceToGCode (z,tp) =
        [ printf "# Slice Z=%1.2f" z
        , printf "G1 Z%1.2f" z
        ] ++ map toolMoveToGCode tp

slicesToGCode :: [(Double, ToolPath)] -> [GCommand]
slicesToGCode slices = 
        prelude ++ (concat $ map sliceToGCode slices) ++ postlude

