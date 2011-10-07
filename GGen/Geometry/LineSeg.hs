{-# LANGUAGE TemplateHaskell #-}

module GGen.Geometry.LineSeg ( invertLineSeg
                             , lineSegDispl
                             , lineSeg2Displ
                             , mergeLineSegs
                             , mergeLineSegs'
                             , GGen.Geometry.LineSeg.runTests
                             ) where

import Data.List ((\\), foldl')
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.VectorSpace
import GGen.Geometry.Types

import Test.QuickCheck.All
import Test.QuickCheck.Property
import Data.VectorSpace.QuickCheck

-- | Reverse the order of line segment termini
invertLineSeg :: LineSeg -> LineSeg
invertLineSeg (LineSeg a b) = LineSeg b a

-- | Displacement of a line segment
lineSegDispl :: LineSeg -> Vec
lineSegDispl (LineSeg a b) = b ^-^ a

-- | Displacement of a line segment
lineSeg2Displ :: LineSeg2 -> Vec2
lineSeg2Displ (LineSeg2 a b) = b ^-^ a

-- | Try merging two line segments
tryMergeLineSegs :: LineSeg -> LineSeg -> Maybe LineSeg
tryMergeLineSegs a b =
        let perms = [ (a,b)
                    , (invertLineSeg a, b)
                    , (a, invertLineSeg b)
                    , (invertLineSeg a, invertLineSeg b) ]
            f (LineSeg a1 a2, LineSeg b1 b2) =
                    if a1 `coincident` b1 then Just $ LineSeg a2 b2
                                          else Nothing
            merged = mapMaybe f perms
            par = parallel (lineSegDispl a) (lineSegDispl b)
        in if par && (not $ null merged) then Just $ head merged
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
prop_merge_divided :: LineSeg -> Normalized Double -> Bool -> Bool -> Result
prop_merge_divided l (Normalized s) flipA flipB
        | magnitude (lineSegDispl l) == 0   = rejected
        | otherwise = case m of
                           Just l    -> if passed then succeeded
                                                  else failed {reason="Deviation too large"}
                           Nothing   -> failed {reason="Not merged"}
                      where f = lerp (lsBegin l) (lsEnd l)
                            a = (if flipA then invertLineSeg else id) $ LineSeg (f 0) (f s)
                            b = (if flipB then invertLineSeg else id) $ LineSeg (f s) (f 1)
                            m = tryMergeLineSegs a b
                            diffBegin = magnitude $ lsBegin l ^-^ lsBegin (fromJust m)
                            diffEnd   = magnitude $ lsEnd l ^-^ lsEnd (fromJust m)
                            passed = diffBegin < 1e-5 && diffEnd < 1e-5

-- | Make sure segments which aren't parallel aren't merged
prop_dont_merge_nonparallel :: LineSeg -> LineSeg -> Result
prop_dont_merge_nonparallel a b
        | abs (normalized (lineSegDispl a) <.> normalized (lineSegDispl b) - 1) == 0    = rejected
        | otherwise = case tryMergeLineSegs a b of
                                Just _    -> failed {reason="Inappropriate merge"}
                                Nothing   -> succeeded

runTests = $quickCheckAll

