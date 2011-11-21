import GGen

infillOffset = 0
infillSpacing = 1.5

settings = GGenSettings
        { ggSliceZStep = 0.4
        , ggInfillPattern = hexInfill infillOffset infillSpacing
        , ggGCodeSettings = gcodeSettings
        }

layerPostlude 2 _ = [ "F300" ]
layerPostlude 5 _ = [ "M104 S225"
                    , "M140 S60"
                    ]
layerPostlude _ _ = []

gcodeSettings = GCodeSettings
        { gcFilamentDia = 3
        , gcSlipRate = 2
        , gcExtrusionDia = 0.4

        , gcDryFeedrate = 600
        , gcExtrudeFeedrate = 300

        , gcPrelude = [ comment "Begin prelude"
                      , "M140 S100"
                      , "M104 S235"
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
        , gcLayerPrelude = (\_ _ -> [])
        , gcLayerPostlude = layerPostlude
        , gcPostlude = [ comment "Begin postlude"
                       , "G161 X0 Y0"
                       , "G161 Z0"
                       , "G1 Z5"
                       , "M104 S0"
                       , "M140 S0"
                       , comment "End postlude"
                       ]
        , gcRetractMinDist = 2 -- millimeter
        , gcRetractLength = 1 -- millimeter
        , gcRetractRate = 1000 -- millimeter/min
        }

main = ggenMain settings
