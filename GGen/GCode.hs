{-# LANGUAGE OverloadedStrings #-}
                
module GGen.GCode ( slicesToGCode
                  , GCommand
                  , GCodeSettings(..)
                  , comment
                  , command
                  ) where

import           Control.Monad (when)
import           Control.Monad.Trans.RWS
import           Data.Monoid
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder.RealFloat
import           Data.VectorSpace

import           GGen.Geometry.Types
import           GGen.Types

type GCodeM = RWS GCodeSettings Builder GCodeState
     
type GCommand = Builder

-- | Amount of filament axis motion to extrude the given volume
eLength :: GCodeSettings -> Double -> Double
eLength settings v = v / filamentArea * gcSlipRate settings
        where -- | Cross sectional area of filament
              filamentArea = pi * (gcFilamentDia settings / 2)**2

command :: Builder -> GCodeM ()
command s = tell $ s<>"\n"
        
comment :: Builder -> GCodeM ()
comment s = tell $ "; "<>s<>"\n"

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
        , gcPrelude :: GCodeM () -- | Commands at beginning of G-code output
        , gcLayerPrelude :: Int -> Double -> GCodeM () -- | Commands at beginning of each layer
        , gcLayerPostlude :: Int -> Double -> GCodeM () -- | Commands at end of each layer
        , gcPostlude :: GCodeM () -- | Commands at end of G-code output

        -- * Retraction options
        , gcRetractMinDist :: Double -- | Minimum move distance to retract
        , gcRetractLength :: Double -- | Amount to retract by
        , gcRetractRate :: Double -- | Retraction feedrate
        }

data GCodeState = GCodeState { gsRetracted :: Bool
                             }
                deriving (Show, Eq)

toolMoveToGCode :: ToolMove -> GCodeM ()
toolMoveToGCode (ToolMove ls@(LineSeg _ end) Dry) =
        do settings <- ask
           state <- get
           when (gcRetractLength settings /= 0 
                 && not (gsRetracted state)
                 && magnitude (lsDispl ls) > gcRetractMinDist settings) $ do
                   put state {gsRetracted=True}
                   command $ "G1E-"<>realFloat (gcRetractLength settings)
                             <>"F"<>realFloat (gcRetractRate settings)
        
           let (x,y) = unp2 end
           command $ "G1X"<>realFloat x<>"Y"<>realFloat y
                     <>"F"<>realFloat (gcDryFeedrate settings)

toolMoveToGCode (ToolMove ls@(LineSeg _ end) (Extrude e)) =
        do settings <- ask
           state <- get
           when (gsRetracted state) $ do
              put state {gsRetracted=False}
              command $ "G1 E"<>realFloat (gcRetractLength settings)
                        <> "F"<>realFloat (gcRetractRate settings)
           let extrusionArea = pi * (gcExtrusionDia settings / 2)**2
               eVol = e * magnitude (lsDispl ls) * extrusionArea
               (x,y) = unp2 end
           command $ "G1X"<>realFloat x<>"Y"<>realFloat y
                     <>"E"<>realFloat (eLength settings eVol)
                     <>"F"<>realFloat (gcExtrudeFeedrate settings)

sliceToGCode :: Int -> (Double, ToolPath) -> GCodeM ()
sliceToGCode layerN (z,tp) = do
     settings <- ask
     (gcLayerPrelude settings) layerN z 
     comment $ "Slice Z="<>realFloat z
     command $ "G1 Z"<>realFloat z
     mapM_ toolMoveToGCode tp
     (gcLayerPostlude settings) layerN z
     
slicesToGCode' :: [(Double, ToolPath)] -> GCodeM ()
slicesToGCode' slices = do
     settings <- ask
     gcPrelude settings
     mapM_ (uncurry sliceToGCode) $ zip [1..] slices
     gcPostlude settings

slicesToGCode :: GCodeSettings -> [(Double, ToolPath)] -> T.Text
slicesToGCode settings slices =
     let (_,w) = evalRWS (slicesToGCode' slices) settings s0
         s0 = GCodeState {gsRetracted=False}
     in toLazyText w


