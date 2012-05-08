{-# LANGUAGE OverloadedStrings #-}
                
import GGen
import GGen.GCode.Extruder       

infillOffset = 0
infillSpacing = 1.5

settings = GGenSettings
        { ggSliceZStep = 0.4
        , ggInfillPattern = hexInfill infillOffset infillSpacing
        , ggToolSettings = gcodeSettings
        }

layerPostlude 2 _ = do command "F300"
layerPostlude 5 _ = do command "M104 S225"
                       command "M140 S60"
layerPostlude _ _ = return ()
              
layerPrelude _ _ = return ()

gcodeSettings = ExtruderSettings
        { gcFilamentDia = 3
        , gcSlipRate = 2
        , gcExtrusionDia = 0.4

        , gcDryFeedrate = 600
        , gcExtrudeFeedrate = 300

        , gcPrelude = do
            comment "Begin prelude"
            command "M140 S100"
            command "M104 S235"
            command "F 1000"
            command "G1 Z5"
            command "G161 X0 Y0 Z0" 
            command "G1 Z1"
            command "G1 X80 Y50"
            command "G1 Z0.30"
            command "G92 X0 Y0 Z0"
            command "F 300"
            comment "End prelude"
        , gcLayerPrelude = layerPrelude
        , gcLayerPostlude = layerPostlude
        , gcPostlude = do 
             comment "Begin postlude"
             command "G161 X0 Y0"
             command "G161 Z0"
             command "G1 Z5"
             command "M104 S0"
             command "M140 S0"
             comment "End postlude"
        , gcRetractMinDist = 2 -- millimeter
        , gcRetractLength = 1 -- millimeter
        , gcRetractRate = 1000 -- millimeter/min
        }

main = ggenMain settings
