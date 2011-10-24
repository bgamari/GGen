{-# LANGUAGE TypeSynonymInstances #-}

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

instance Vector Vec3 where
        vec (x,y,z) = parens $ hcat $ punctuate (text ", ") $ map approxDouble [x,y,z]

instance Vector Vec2 where
        vec (x,y) = parens $ hcat $ punctuate (text ", ") $ map approxDouble [x,y]

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

