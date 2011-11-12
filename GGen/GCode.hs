module GGen.GCode ( slicesToGCode
                  , GCommand
                  , GCodeSettings(..)
                  , comment
                  ) where

import Text.Printf
import Data.VectorSpace
import Control.Monad.Trans.State

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

data GCodeSettings = GCodeSettings
        { gcPrelude :: [GCommand]
        , gcLayerPrelude :: Int -> Double -> [GCommand]
        , gcLayerPostlude :: Int -> Double -> [GCommand]
        , gcPostlude :: [GCommand]
        , gcRetractMinDist :: Double
        , gcRetractLength :: Double
        , gcRetractRate :: Double
        } deriving (Show, Eq)

-- TODO: Retract filament
toolMoveToGCode :: GCodeSettings -> ToolMove -> State Bool [GCommand]
toolMoveToGCode settings (ToolMove ls@(LineSeg _ (P (x,y))) Dry) =
        do retracted <- get
           let move = printf "G1 X%1.3f Y%1.3f" x y 
           if    gcRetractLength settings /= 0 
              && not retracted
              && magnitude (lsDispl ls) > gcRetractMinDist settings
              then do put True
                      return [ printf "G1 E-%f F%f" (gcRetractLength settings) (gcRetractRate settings)
                             , move ]
              else return [ move ]

toolMoveToGCode settings (ToolMove ls@(LineSeg _ (P (x,y))) (Extrude e)) =
        do retracted <- get
           let eVol = e * magnitude (lsDispl ls) * extrusionArea
               move = printf "G1 X%1.3f Y%1.3f E%1.3f" x y (eLength eVol)
           if retracted
              then do put False
                      return [ printf "G1 E%f F%f" (gcRetractLength settings) (gcRetractRate settings)
                             , move ]
              else return [ move ]

sliceToGCode :: GCodeSettings -> Int -> (Double, ToolPath) -> [GCommand]
sliceToGCode settings layerN (z,tp) =
        (gcLayerPrelude settings) layerN z 
     ++ [ comment $ printf "Slice Z=%1.2f" z
        , printf "G1 Z%1.2f" z ]
     ++ concat (evalState (mapM (toolMoveToGCode settings) tp) False)
     ++ (gcLayerPostlude settings) layerN z

slicesToGCode :: GCodeSettings -> [(Double, ToolPath)] -> [GCommand]
slicesToGCode settings slices =
        gcPrelude settings
     ++ (concat $ zipWith (sliceToGCode settings) [1..] slices)
     ++ gcPostlude settings

