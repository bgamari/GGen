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
extrusionDia = 0.4 -- millimeters

-- | The ratio of actual feed distance to E axis distance (0 means completely
-- slipping, 1 means no slip)
eSlipRate = 2

-- | Cross sectional area of extrudant
extrusionArea = pi * (extrusionDia/2)**2

-- | Amount of filament axis motion to extrude the given volume
eLength v = v / filamentArea * eSlipRate

comment s = "; " ++ s

prelude = [ comment "Begin prelude"
          , "F 1000"
	  , "G1 Z5"
          , "G161 X0 Y0 Z0" 
	  , "G1 Z1"
          , "G1 X80 Y50"
	  , "G1 Z0.30"
          , "G92 X0 Y0 Z0"
          , "F 300"
          , comment "End prelude"
          ]

layer1Postlude = [ comment "Layer 1 postlude"
                 , "F500"
		 , comment "End layer 1 postlude"
		 ]

postlude = [ comment "Begin postlude"
          , "G1 Z20"
          , "G161 X0 Y0"
          , "M104 S0"
          , "M140 S0"
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
slicesToGCode slices = prelude
                    ++ sliceToGCode (head slices)
		    ++ layer1Postlude
		    ++ (concat $ map sliceToGCode $ tail slices)
		    ++ postlude

