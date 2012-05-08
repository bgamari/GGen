module GGen.ToolPath.Extruder ( ExtruderMove(..)
                              , ExtruderMarker
                              ) where

-- | Specifies whether the extruder should extrude during a move
data ExtruderMove = Extrude Double
                  | Dry
                  deriving (Show, Eq)

data ExtruderMarker = LayerChange Int
                    deriving (Show, Eq)

