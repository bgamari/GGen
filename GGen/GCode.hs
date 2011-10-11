module GGen.GCode ( slicesToGCode
                  , GCommand
                  ) where

import Text.Printf
import Data.VectorSpace

import GGen.Geometry.Types
import GGen.Types

type GCommand = String

prelude = [ "# Begin prelude"
          , "G161 X0 Y0 Z0" 
          , "# End prelude"
          ]

postlude = [ "# Begin postlude"
          , "G1 Z10"
          , "G1 X0 Y0"
          , "# End postlude"
          ]

eFeed = 1

-- TODO: Retract filament
toolMoveToGCode :: ToolMove -> GCommand
toolMoveToGCode (ToolMove (LineSeg _ (x,y)) Dry) =
        printf "G1 X%1.2f Y%1.2f" x y
toolMoveToGCode (ToolMove l@(LineSeg _ (x,y)) (Extrude a)) =
        printf "G1 X%1.2f Y%1.2f E%1.2f" x y (a*eFeed*magnitude (lsDispl l))

sliceToGCode :: (Double, ToolPath) -> [GCommand]
sliceToGCode (z,tp) =
        [ printf "# Slice Z=%1.2f" z
        , printf "G1 Z%1.2f" z
        ] ++ map toolMoveToGCode tp

slicesToGCode :: [(Double, ToolPath)] -> [GCommand]
slicesToGCode slices = 
        prelude ++ (concat $ map sliceToGCode slices) ++ postlude

