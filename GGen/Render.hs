module GGen.Render (
                   -- Drawing entity paths
                     drawPolygon2
                   , drawSegment2

                   -- Rendering entities
                   , renderPath2
                   , renderArrow
                   , renderPolygons2
                   , renderOrientedPolygons
                   , renderToolpath
                   , renderPoint2

                   -- Utility
                   , renderRegionToSVG
                   ) where

import Graphics.Rendering.Cairo
import Data.VectorSpace
import Data.AffineSpace

import GGen.Geometry.Types
import GGen.Types

-- | Rescale and center a surface of size (w,h) for region (rmin,rmax)
rescaleForRegion :: (Double, Double) -> Box Vec3 -> Render ()
rescaleForRegion (w,h) (rmin,rmax) = 
        do scale w h
           translate 0.5 0.5
           scale s s
           translate (-cx) (-cy)
           setLineWidth 4e-1
        where (rw, rh, _) = rmax .-. rmin
              viewRatio = w/h
              regionRatio = rw/rh
              P (cx, cy, _) = rmin .+^ (rmax .-. rmin) ^/ 2
              widthConstrained = viewRatio < regionRatio
              s = if widthConstrained then 1/rw
                                      else 1/rh

-- | Draws a line segment path
drawSegment :: LineSeg Vec3 -> Render ()
drawSegment (LineSeg (P (ux,uy,_)) (P (vx,vy,_))) =
        do lineTo ux uy
           lineTo vx vy

moveToPt, lineToPt :: Point2 -> Render ()
lineToPt (P (x,y)) = lineTo x y
moveToPt (P (x,y)) = moveTo x y

-- | Draws a line segment path
drawSegment2 :: LineSeg Vec2 -> Render ()
drawSegment2 (LineSeg u v) = lineToPt u >> lineToPt v


renderPath2 :: LineSegPath Vec2 -> Render ()
renderPath2 path = 
        do newPath
           mapM_ drawSegment2 path
           stroke

renderArrow :: LineSeg Vec2 -> Render ()
renderArrow l@(LineSeg a b) =
        do drawSegment2 l
           stroke
           let size = magnitude $ lsDispl l
               (dx,dy) = a .-. b
               angle = atan2 dy dx
               aLength = 0.15 * size
               aAngle = 20 * pi / 180
               f a = (cos a, sin a)
           moveToPt b
           lineToPt $ b .-^ aLength *^ f (angle-aAngle)
           lineToPt $ b .-^ aLength *^ f (angle+aAngle)
           lineToPt b
           fill 

renderRegionToSVG :: FilePath -> (Double,Double) -> Box Vec3 -> Render () -> IO ()
renderRegionToSVG filename (w,h) region action =
        withSVGSurface filename w h (flip renderWith $ do rescaleForRegion (w,h) region
                                                          action
                                                          showPage)

-- | Render polygons
renderPolygons2 :: [Polygon Vec2] -> Render ()
renderPolygons2 polys =
        do newPath
           setFillRule FillRuleEvenOdd
           setSourceRGBA 0 0 1 0.5
           mapM_ drawPolygon2 polys
           fill

           newPath
           setSourceRGBA 0 0 0 1
           mapM_ drawPolygon2 polys
           stroke

-- | Draw a 2D polygon
drawPolygon2 :: Polygon Vec2 -> Render ()
drawPolygon2 (Polygon points) = do moveToPt $ head points
                                   mapM_ lineToPt points
                                   lineToPt $ head points

-- | Draw a polygon path
drawPolygon :: Polygon Vec3 -> Render ()
drawPolygon (Polygon points) = do moveTo x y
                                  mapM_ (\(P (x,y,z)) -> lineTo x y) points
                                  lineTo x y
                               where P (x,y,_) = head points

-- | Render 2D oriented polygons
renderOrientedPolygons :: [OrientedPolygon Vec2] -> Render ()
renderOrientedPolygons polys =
        do setFillRule FillRuleEvenOdd
           newPath
           setSourceRGBA 0 0 1 0.3
           mapM_ (drawPolygon2 . fst) polys
           fill

           newPath
           setSourceRGBA 1 0 0 1
           mapM_ (drawPolygon2 . fst) $ filter (\(_,hand) -> hand==RightHanded) polys
           stroke

           newPath
           setSourceRGBA 0 0 0 1
           mapM_ (drawPolygon2 . fst) $ filter (\(_,hand) -> hand==LeftHanded) polys
           stroke

renderToolpath :: ToolPath -> Render ()
renderToolpath tp =
        do newPath
           let f (ToolMove l (Extrude _)) = setSourceRGBA 0 0 1 0.6 >> renderArrow l >> stroke
               f (ToolMove l Dry) = setSourceRGBA 0 1 0 0.6 >> renderArrow l >> stroke
           mapM_ f tp

renderPoint2 :: Point2 -> Render ()
renderPoint2 (P (x,y)) =
        arc x y 0.5 0 (2*pi) >> fill

