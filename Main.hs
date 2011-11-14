import GGen

infillOffset = 0
infillSpacing = 1.5

settings = GGenSettings
        { ggSliceZStep = 0.4
        , ggInfillPattern = hexInfill infillOffset infillSpacing
        , ggGCodeSettings = gcodeSettings
        }

layerPostlude 1 _ = [ "F600" ]
layerPostlude _ _ = []

gcodeSettings = GCodeSettings
	{ gcPrelude = [ comment "Begin prelude"
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
	, gcRetractRate = 200 -- millimeter/min
	}

main = ggenMain settings
