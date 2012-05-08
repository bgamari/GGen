module GGen.ToolPath.Types ( ToolPath(..)
                           , ToolMove(..)
                           , PreserveOrder(..)
                           ) where

import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import           GGen.Geometry.Types

data PreserveOrder = Ordered | Unordered deriving (Show, Eq)

data ToolMove m t = ToolMove { tmMove :: LineSeg R2
                             , tmToolConfig :: t
                             }                         -- | A tool movement
                  | Marker m                           -- | A marker indicating an event (e.g. slice change)
                  deriving (Show, Eq)
                  
-- | A sequence of ToolMoves. A ToolPath must be executed atomically and 
-- segments of paths denoted as Ordered must not be reordered (up to reflection)
data ToolPath m t
     = PathStep (ToolMove m t)                      -- | A step of a path
     | ToolPath PreserveOrder (Seq (ToolPath m t))  -- | A set of paths without an ordering
     deriving (Eq, Show)

concatOrdered :: ToolPath m t -> ToolPath m t -> ToolPath m t
(ToolPath Ordered a) `concatOrdered` (ToolPath Ordered b) = ToolPath Ordered $ a >< b
a `concatOrdered` b = ToolPath Ordered $ S.fromList [a,b]

concatUnordered :: ToolPath m t -> ToolPath m t -> ToolPath m t
a `concatUnordered` b = ToolPath Unordered $ S.fromList [a,b]

