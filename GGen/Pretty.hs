module GGen.Pretty where

import Numeric.LinearAlgebra
import Text.PrettyPrint.HughesPJ as PP
import GGen.Geometry.Types

approx :: Int -> Double -> Double
approx places x = (realToFrac $ round $ 10^places*x) / 10^places

approxDouble = PP.double . approx 2

vec :: Vec -> Doc
vec v = parens $ hcat $ punctuate (text ", ") $ map approxDouble $ toList v
point = vec

lineSeg (LineSeg a b) = text "segment" <+> vec a <+> text "--" <+> vec b

line (Line a m) = text "line" <+> vec a <+> text "->" <+> vec m

plane (Plane {planeNormal=n, planePoint=v}) =
        text "plane" <+> text "through" <+> vec v <+> text "with normal" <+> vec n

face (Face {faceVertices=(v0,v1,v2)}) =
        text "face" <+> vec v0 <+> vec v1 <+> vec v2

polygon p = text "polygon" <+> (hsep $ map vec p)

orientedPolygon (p,True) = text "filled" <+> polygon p
orientedPolygon (p,False) = text "unfilled" <+> polygon p

