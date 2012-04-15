module GGen.ToolPath.Types ( Extrude(..)
                           , ToolMove(..)
                           , ToolPath(..)
                           , PreserveOrder(..)
                           , tpBegin
                           , tpEnd
                           , tpInvert
                           ) where

import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import           GGen.Geometry.Types

-- | Specifies whether the extruder should extrude during a move
data Extrude = Extrude Double
             | Dry
             deriving (Show, Eq)

data PreserveOrder = Ordered | Unordered deriving (Show, Eq)

-- | A sequence of ToolMoves. A ToolPath must be executed atomically and 
-- segments of paths denoted as Ordered must not be reordered
data ToolPath m t
     = ToolMove { tpMove :: LineSeg R2
                , tpToolConfig :: t
                }                                  -- | A tool movement
     | ToolPath PreserveOrder (Seq (ToolPath m t)) -- | A set of paths without an ordering
     | Marker m                                    -- | A marker indicating an event (e.g. slice change)
     deriving (Eq, Show)

-- | Start point of a toolpath
tpBegin :: ToolPath m t -> Maybe P2
tpBegin (ToolMove {tpMove=ls}) = Just $ lsA ls
tpBegin (OrderedPath Unordered s) = case viewl $  
                        | otherwise = let a :< _ = viewl s in tpBegin a
tpBegin (Marker m) = Nothing

-- | End point of a toolpath
tpEnd :: ToolPath m t -> P2
tpEnd [] = error "Empty toolpath has no end"
tpEnd tp = p where ToolMove (LineSeg _ p) _ = last tp

-- | Reverse a toolpath
tpInvert = reverse . map (\(ToolMove l e)->ToolMove (lsInvert l) e)

