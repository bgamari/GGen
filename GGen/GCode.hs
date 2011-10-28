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

-- | Cross sectional area of extrudant
extrusionArea = pi * (extrusionDia/2)**2

-- | Amount of filament axis motion to extrude the given volume
eLength v = v / filamentArea / eSlipRate

comment s = "; " ++ s

prelude = [ comment "Begin prelude"
          , "G161 X0 Y0 Z0" 
          , comment "End prelude"
          ]

postlude = [ comment "Begin postlude"
          , "G1 Z10"
          , "G1 X0 Y0"
          , comment "End postlude"
          ]

-- TODO: Retract filament
toolMoveToGCode :: ToolMove -> GCommand
toolMoveToGCode (ToolMove (LineSeg _ (P (x,y))) Dry) =
        printf "G1 X%1.3f Y%1.3f" x y
toolMoveToGCode (ToolMove l@(LineSeg _ (P (x,y))) (Extrude e)) =
        printf "G1 X%1.3f Y%1.3f E%1.3f" x y (eLength eVol)
        where eVol = e * magnitude (lsDispl l) * extrusionArea

sliceToGCode :: (Double, ToolPath) -> [GCommand]
sliceToGCode (z,tp) =
        [ comment $ printf "Slice Z=%1.2f" z
        , printf "G1 Z%1.2f" z
        ] ++ map toolMoveToGCode tp

slicesToGCode :: [(Double, ToolPath)] -> [GCommand]
slicesToGCode slices = 
        prelude ++ (concat $ map sliceToGCode slices) ++ postlude

