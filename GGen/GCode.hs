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

-- | Amount of filament axis motion to extrude the given volume
eLength :: GCodeSettings -> Double -> Double
eLength settings v = v / filamentArea * gcSlipRate settings
        where -- | Cross sectional area of filament
              filamentArea = pi * (gcFilamentDia settings / 2)**2

comment s = "; " ++ s

data GCodeSettings = GCodeSettings
        {
        -- * Extrusion parameters
          gcFilamentDia :: Double -- | Diameter of filament
        , gcSlipRate :: Double -- | The ratio of actual feed distance to E axis
                               -- distance (0 means completely slipping, 1
                               -- means no slip)
        , gcExtrusionDia :: Double -- | Diameter of extruded filament

        -- * Feedrates
        , gcDryFeedrate :: Double -- | Feedrate during dry move
        , gcExtrudeFeedrate :: Double -- | Feedrate during extrusion

        -- * G-code output
        , gcPrelude :: [GCommand] -- | Commands at beginning of G-code output
        , gcLayerPrelude :: Int -> Double -> [GCommand] -- | Commands at beginning of each layer
        , gcLayerPostlude :: Int -> Double -> [GCommand] -- | Commands at end of each layer
        , gcPostlude :: [GCommand] -- | Commands at end of G-code output

        -- * Retraction options
        , gcRetractMinDist :: Double -- | Minimum move distance to retract
        , gcRetractLength :: Double -- | Amount to retract by
        , gcRetractRate :: Double -- | Retraction feedrate
        } deriving (Show, Eq)

toolMoveToGCode :: GCodeSettings -> ToolMove -> State Bool [GCommand]
toolMoveToGCode settings (ToolMove ls@(LineSeg _ (P (x,y))) Dry) =
        do retracted <- get
           let move = printf "G1 X%1.3f Y%1.3f F%1.3" x y (gcDryFeedrate settings)
           if    gcRetractLength settings /= 0 
              && not retracted
              && magnitude (lsDispl ls) > gcRetractMinDist settings
              then do put True
                      return [ printf "G1 E-%f F%f" (gcRetractLength settings) (gcRetractRate settings)
                             , move ]
              else return [ move ]

toolMoveToGCode settings (ToolMove ls@(LineSeg _ (P (x,y))) (Extrude e)) =
        do retracted <- get
           let extrusionArea = pi * (gcExtrusionDia settings / 2)**2
               eVol = e * magnitude (lsDispl ls) * extrusionArea
               move = printf "G1 X%1.3f Y%1.3f E%1.3f F%1.3" x y (eLength settings eVol) (gcExtrudeFeedrate settings) (gcExtrudeFeedrate settings)
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

