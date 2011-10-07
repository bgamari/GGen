{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies #-}

module GGen.Geometry.LineSeg ( mergeLineSegs
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

-- | Try merging two line segments
tryMergeLineSegs :: (InnerSpace p, RealFloat (Scalar p)) => LineSeg p -> LineSeg p -> Maybe (LineSeg p)
tryMergeLineSegs a b =
        let perms = [ (a,b)
                    , (lsInvert a, b)
                    , (a, lsInvert b)
                    , (lsInvert a, lsInvert b) ]
            f (LineSeg a1 a2, LineSeg b1 b2) =
                    if a1 `coincident` b1 then Just $ LineSeg a2 b2
                                          else Nothing
            merged = mapMaybe f perms
            par = parallel (lsDispl a) (lsDispl b)
        in if par && (not $ null merged) then Just $ head merged
                                         else Nothing

-- | Merge two line segments if possible, otherwise return both segments
mergeLineSegs :: (InnerSpace p, RealFloat (Scalar p)) => LineSeg p -> LineSeg p -> [LineSeg p]
mergeLineSegs a b = maybe [a,b] (replicate 1) $ tryMergeLineSegs a b

-- | Merge a line segment into a list, return list with new segment either merged or added
mergeLineSegIntoList :: (InnerSpace p, Eq p, RealFloat (Scalar p)) => [LineSeg p] -> LineSeg p -> [LineSeg p]
mergeLineSegIntoList ls l =
        let tryMerge l' = do new <- tryMergeLineSegs l l'
                             return (new, ls \\ [l])
            tries = mapMaybe tryMerge ls
            (lMerged, ls') = head tries
        in if null tries then l:ls
                         else lMerged:ls'

-- | Merge line segments in a list
mergeLineSegs' :: (InnerSpace p, Eq p, RealFloat (Scalar p)) => [LineSeg p] -> [LineSeg p]
mergeLineSegs' ls = foldl' mergeLineSegIntoList [] ls

-- QuickCheck properties
prop_invert_displacement :: LineSeg Point -> Bool
prop_invert_displacement l = (lsDispl $ lsInvert l) == (negateV $ lsDispl l)

-- | Split a line segment in two and ensure that the pieces are merged
prop_merge_divided :: LineSeg Point -> Normalized Double -> Bool -> Bool -> Result
prop_merge_divided l (Normalized s) flipA flipB
        | magnitude (lsDispl l) == 0   = rejected
        | otherwise = case m of
                           Just l    -> if passed then succeeded
                                                  else failed {reason="Deviation too large"}
                           Nothing   -> failed {reason="Not merged"}
                      where f = lerp (lsA l) (lsB l)
                            a = (if flipA then lsInvert else id) $ LineSeg (f 0) (f s)
                            b = (if flipB then lsInvert else id) $ LineSeg (f s) (f 1)
                            m = tryMergeLineSegs a b
                            diffBegin = magnitude $ lsA l ^-^ lsA (fromJust m)
                            diffEnd   = magnitude $ lsB l ^-^ lsB (fromJust m)
                            passed = diffBegin < 1e-5 && diffEnd < 1e-5

-- | Make sure segments which aren't parallel aren't merged
prop_dont_merge_nonparallel :: LineSeg Point -> LineSeg Point -> Result
prop_dont_merge_nonparallel a b
        | abs (normalized (lsDispl a) <.> normalized (lsDispl b) - 1) == 0    = rejected
        | otherwise = case tryMergeLineSegs a b of
                                Just _    -> failed {reason="Inappropriate merge"}
                                Nothing   -> succeeded

runTests = $quickCheckAll

