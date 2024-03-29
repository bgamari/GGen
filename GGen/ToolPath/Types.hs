module GGen.ToolPath.Types ( Extrude(..)
                           , ToolMove(..)
                           , ToolPath
                           , tpBegin
                           , tpEnd
                           , tpInvert
                           ) where

import GGen.Geometry.Types

-- | Specifies whether the extruder should extrude during a move
data Extrude = Extrude Double
             | Dry
             deriving (Show, Eq)

-- | Specifies a tool motion within a slice
data ToolMove = ToolMove (LineSeg R2) Extrude
              deriving (Show)

instance ApproxEq ToolMove where
        (ToolMove l e) `approx` (ToolMove l' e')  = l `approx` l' && e == e'

-- | A sequence of ToolMoves. Line segments of successive moves should connect.
type ToolPath = [ToolMove]

tpBegin, tpEnd :: ToolPath -> P2
-- | Start point of a toolpath
tpBegin [] = error "Empty toolpath has no beginning"
tpBegin tp = p where ToolMove (LineSeg p _) _ = head tp
-- | End point of a toolpath
tpEnd [] = error "Empty toolpath has no end"
tpEnd tp = p where ToolMove (LineSeg _ p) _ = last tp

-- | Reverse a toolpath
tpInvert = reverse . map (\(ToolMove l e)->ToolMove (lsInvert l) e)

