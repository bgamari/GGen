{-# LANGUAGE OverloadedStrings #-}

module GGen.GCode.Extruder ( slicesToGCode
                           , ExtruderSettings(..)
                           , ExtruderM
                           ) where
                           
import           Control.Monad (when)
import           Control.Monad.Trans.RWS
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder.RealFloat
import           Data.VectorSpace
import           Prelude hiding (mapM_)

import           Data.Sequence (Seq)                 
import qualified Data.Sequence as SQ                 

import           GGen.GCode
import           GGen.ToolPath
import           GGen.ToolPath.Extruder
import           GGen.Geometry.Types
import           GGen.Types

data ExtruderSettings = ExtruderSettings
        {
        -- * Extrusion parameters
          gcFilamentDia :: Double           -- | Diameter of filament
        , gcSlipRate :: Double              -- | The ratio of actual feed distance to E axis
                                            -- distance (0 means completely slipping, 1
                                            -- means no slip)
        , gcExtrusionDia :: Double          -- | Diameter of extruded filament

        -- * Feedrates
        , gcDryFeedrate :: Double           -- | Feedrate during dry move
        , gcExtrudeFeedrate :: Double       -- | Feedrate during extrusion

        -- * G-code output
        , gcPrelude :: ExtruderM ()         -- | Commands at beginning of G-code output
        , gcLayerPrelude :: Int -> Double -> ExtruderM ()  -- | Commands at beginning of each layer
        , gcLayerPostlude :: Int -> Double -> ExtruderM () -- | Commands at end of each layer
        , gcPostlude :: ExtruderM ()        -- | Commands at end of G-code output

        -- * Retraction options
        , gcRetractMinDist :: Double        -- | Minimum move distance to retract
        , gcRetractLength :: Double         -- | Amount to retract by
        , gcRetractRate :: Double           -- | Retraction feedrate
        }

data ExtruderState = ExtruderState { gsRetracted :: Bool
                                   }
                   deriving (Show, Eq)

type ExtruderM = GCodeM ExtruderSettings ExtruderState

showReal = formatRealFloat Fixed (Just 3)

setRetracted :: Bool -> ExtruderM ()
setRetracted retracted =
        do settings <- ask
           state <- get
           when (gcRetractLength settings /= 0 
                 && gsRetracted state /= retracted) $ do
                put state {gsRetracted=True}
                case retracted of
                     True ->  command $ "G1E-"<>showReal (gcRetractLength settings)
                                        <>"F"<>showReal (gcRetractRate settings)
                     False -> command $ "G1 E"<>showReal (gcRetractLength settings)
                                        <> "F"<>showReal (gcRetractRate settings)
                
-- | Amount of filament axis motion to extrude the given volume
eLength :: ExtruderSettings -> Double -> Double
eLength settings v = v / filamentArea * gcSlipRate settings
        where -- | Cross sectional area of filament
              filamentArea = pi * (gcFilamentDia settings / 2)**2

toolMoveToGCode :: ToolMove m ExtruderMove -> ExtruderM ()
toolMoveToGCode (ToolMove ls@(LineSeg _ end) Dry) =
        do settings <- ask
           state <- get
           when (magnitude (lsDispl ls) > gcRetractMinDist settings)
                $ setRetracted True
           let (x,y) = unp2 end
           command $ "G1X"<>showReal x<>"Y"<>showReal y
                     <>"F"<>showReal (gcDryFeedrate settings)

toolMoveToGCode (ToolMove ls@(LineSeg _ end) (Extrude e)) =
        do settings <- ask
           state <- get
           setRetracted False
           let extrusionArea = pi * (gcExtrusionDia settings / 2)**2
               eVol = e * magnitude (lsDispl ls) * extrusionArea
               (x,y) = unp2 end
           command $ "G1X"<>showReal x<>"Y"<>showReal y
                     <>"E"<>showReal (eLength settings eVol)
                     <>"F"<>showReal (gcExtrudeFeedrate settings)

sliceToGCode :: Int -> (Double, ToolPath m ExtruderMove) -> ExtruderM ()
sliceToGCode layerN (z,tp) = do
     settings <- ask
     (gcLayerPrelude settings) layerN z 
     comment $ "Slice Z="<>showReal z
     command $ "G1 Z"<>showReal z
     mapM_ toolMoveToGCode $ flattenPath (p2 (0,0)) tp  -- FIXME: Track position
     (gcLayerPostlude settings) layerN z
     
slicesToGCode' :: [(Double, ToolPath m ExtruderMove)] -> ExtruderM ()
slicesToGCode' slices = do
     settings <- ask
     gcPrelude settings
     mapM_ (uncurry sliceToGCode) $ zip [1..] slices
     gcPostlude settings

slicesToGCode :: ExtruderSettings -> [(Double, ToolPath m ExtruderMove)] -> T.Text
slicesToGCode settings slices =
     let (_,w) = evalRWS (slicesToGCode' slices) settings s0
         s0 = ExtruderState {gsRetracted=False}
     in toLazyText w


