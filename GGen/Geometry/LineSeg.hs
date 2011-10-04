module GGen.Geometry.LineSeg ( invertLineSeg
                             , lineSegDispl
                             , mergeLineSegs
                             , mergeLineSegs'
                             ) where

import Data.List ((\\), foldl')
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.VectorSpace
import Data.VectorSpace.QuickCheck
import GGen.Geometry.Types

-- | Reverse the order of line segment termini
invertLineSeg :: LineSeg -> LineSeg
invertLineSeg (LineSeg a b) = LineSeg b a

-- | Displacement of a line segment
lineSegDispl :: LineSeg -> Vec
lineSegDispl (LineSeg a b) = b ^-^ a

-- | Try merging two line segments
tryMergeLineSegs :: LineSeg -> LineSeg -> Maybe LineSeg
tryMergeLineSegs a b =
        let perms = [ (a,b)
                    , (invertLineSeg a, b)
                    , (a, invertLineSeg b)
                    , (invertLineSeg a, invertLineSeg b) ]
            f (LineSeg a1 a2, LineSeg b1 b2) =
                    if a1 `samePoint` b1 then Just $ LineSeg a2 b2
                                         else Nothing
            dirDev = abs (lineSegDispl a <.> lineSegDispl b) - 1
            merged = mapMaybe f perms
        in if dirDev < dirTol && (not $ null merged) then Just $ head merged
                                                     else Nothing

-- | Merge two line segments if possible, otherwise return both segments
mergeLineSegs :: LineSeg -> LineSeg -> [LineSeg]
mergeLineSegs a b = maybe [a,b] (replicate 1) $ tryMergeLineSegs a b

-- | Merge a line segment into a list, return list with new segment either merged or added
mergeLineSegIntoList :: [LineSeg] -> LineSeg -> [LineSeg]
mergeLineSegIntoList ls l =
        let tryMerge l' = do new <- tryMergeLineSegs l l'
                             return (new, ls \\ [l])
            tries = mapMaybe tryMerge ls
            (lMerged, ls') = head tries
        in if null tries then l:ls
                         else lMerged:ls'

-- | Merge line segments in a list
mergeLineSegs' :: [LineSeg] -> [LineSeg]
mergeLineSegs' ls = foldl' mergeLineSegIntoList [] ls

-- QuickCheck properties
prop_invert_displacement :: LineSeg -> Bool
prop_invert_displacement l = (lineSegDispl $ invertLineSeg l) == (negateV $ lineSegDispl l)

-- | Split a line segment in two and ensure that the pieces are merged
prop_merge_divided :: LineSeg -> Bool
prop_merge_divided l = isJust m && l == fromJust m
                       where f = lerp (lsBegin l) (lsEnd l)
                             a = LineSeg (f 0) (f 0.5)
                             b = LineSeg (f 0.5) (f 1)
                             m = tryMergeLineSegs a b

props = [ prop_invert_displacement
        , prop_merge_divided
        ]
