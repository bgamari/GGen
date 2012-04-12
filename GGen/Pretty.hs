{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GGen.Pretty ( vec
                   , point
                   , lineSeg
                   , line
                   , plane
                   , face
                   , polygon
                   , orientedPolygon
                   , module PP
                   ) where

import Data.VectorSpace
import Text.PrettyPrint.HughesPJ as PP
import GGen.Geometry.Types

approximate :: Int -> Double -> Double
approximate places x = (realToFrac $ round $ 10^places*x) / 10^places

approxDouble = PP.double . approximate 2

class Vector v where
        vec :: v -> Doc

instance Vector R3 where
        vec v = let (x,y,z) = unr3 v
                in parens $ hcat $ punctuate (text ", ") $ map approxDouble [x,y,z]

instance Vector R2 where
        vec v = let (x,y) = unr2 v
                in parens $ hcat $ punctuate (text ", ") $ map approxDouble [x,y]

point (P v) = vec v

lineSeg (LineSeg a b) = text "segment" <+> vec a <+> text "--" <+> vec b

line (Line a m) = text "line" <+> vec a <+> text "->" <+> vec m

plane (Plane {planeNormal=n, planePoint=v}) =
        text "plane" <+> text "through" <+> point v <+> text "with normal" <+> vec n

face (Face {faceVertices=(v0,v1,v2)}) =
        text "face" <+> point v0 <+> point v1 <+> point v2

polygon p = text "polygon" <+> (hsep $ map vec p)

orientedPolygon (p,True) = text "filled" <+> polygon p
orientedPolygon (p,False) = text "unfilled" <+> polygon p

intersection = undefined

