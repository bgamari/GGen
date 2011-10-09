module GGen.Render ( renderPath
                   , renderPath2
                   , renderArrow
                   , renderPathsToSVG
                   , renderRegionToSVG
                   , renderPolygons2
                   , renderOrientedPolygons

                   -- Drawing entity paths
                   , drawPolygon2
                   , drawSegment2
                   ) where

import Graphics.Rendering.Cairo
import GGen.Geometry.Types
import Data.VectorSpace

-- | Rescale and center a surface of size (w,h) for region (rmin,rmax)
rescaleForRegion :: (Double, Double) -> (Point, Point) -> Render ()
rescaleForRegion (w,h) (rmin,rmax) = 
        do scale w h
           translate 0.5 0.5
           scale s s
           translate (-cx) (-cy)
           setLineWidth 4e-1
        where (rminX, rminY, _) = rmin
              (rmaxX, rmaxY, _) = rmax
              (rw, rh) = (rmaxX-rminX, rmaxY-rminY)
              viewRatio = w/h
              regionRatio = rw/rh
              (cx, cy, _) = rmin + (rmax - rmin) ^/ 2
              widthConstrained = viewRatio < regionRatio
              s = if widthConstrained then 1/rw
                                      else 1/rh

renderPath2 :: LineSegPath Point2 -> Render ()
renderPath2 path = 
        do newPath
           mapM_ drawSegment2 $ path
           stroke

-- | Render a line segment path
renderPath :: LineSegPath Point -> (Point,Point) -> Render ()
renderPath path (rmin,rmax) =
        do setSourceRGB 0 0 0
           newPath
           mapM_ drawSegment' $ filter zClipped path
           stroke
        where (_,_,maxZ) = rmax
              (_,_,minZ) = rmin

              depthToAlpha z = 1

              drawSegment' :: LineSeg Point -> Render ()
              drawSegment' l@(LineSeg (_,_,z) _) =
                      do -- This is only correct for lines that don't travel far in Z
                         -- TODO: Fix
                         --setSourceRGBA 0 0 0 $ depthToAlpha ((u-minZ) / (maxZ-minZ))
                         drawSegment l

              zClipped (LineSeg (_,_,az) (_,_,bz))
                       | az > maxZ && bz > maxZ  = False
                       | az < minZ && bz < minZ  = False
                       | otherwise               = True

renderArrow :: LineSeg Point2 -> Render ()
renderArrow l@(LineSeg (ax,ay) (bx,by)) =
        do setSourceRGBA 1 0 0 0.5
           drawSegment2 l
           stroke
           let size = magnitude $ lsDispl l
               angle = atan2 (by-ay) (bx-ax)
               aLength = 0.15 * size
               aAngle = 30 * pi / 180
           moveTo bx by
           lineTo (bx - aLength * cos (angle-aAngle)) (by - aLength * sin (angle-aAngle))
           lineTo (bx - aLength * cos (angle+aAngle)) (by - aLength * sin (angle+aAngle))
           lineTo bx by
           fill 

renderRegionToSVG :: FilePath -> (Double,Double) -> (Point,Point) -> Render () -> IO ()
renderRegionToSVG filename (w,h) region action =
        withSVGSurface filename w h (flip renderWith $ do rescaleForRegion (w,h) region
                                                          action
                                                          showPage)

renderPathsToSVG :: FilePath -> (Double,Double) -> (Point,Point) -> [LineSegPath Point] -> IO ()
renderPathsToSVG filename (w,h) region paths =
        renderRegionToSVG filename (w,h) region (mapM_ (\path->renderPath path region) paths)

-- | Draws a line segment path
drawSegment :: LineSeg Point -> Render ()
drawSegment (LineSeg (ux,uy,uz) (vx,vy,vz)) =
        do lineTo ux uy
           lineTo vx vy

-- | Draws a line segment path
drawSegment2 :: LineSeg Point2 -> Render ()
drawSegment2 (LineSeg (ux,uy) (vx,vy)) =
        do lineTo ux uy
           lineTo vx vy

-- | Render polygons
renderPolygons2 :: [Polygon Point2] -> Render ()
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
drawPolygon2 :: Polygon Point2 -> Render ()
drawPolygon2 (Polygon points) = do (uncurry moveTo) $ head points
                                   mapM_ (uncurry lineTo) points
                                   (uncurry lineTo) $ head points

-- | Draw a polygon path
drawPolygon :: Polygon Point -> Render ()
drawPolygon (Polygon points) = do moveTo x y
                                  mapM_ (\(x,y,z) -> lineTo x y) points
                                  lineTo x y
        where (x,y,_) = head points

-- | Render 2D oriented polygons
renderOrientedPolygons :: [OrientedPolygon Point2] -> Render ()
renderOrientedPolygons polys =
        do setFillRule FillRuleEvenOdd
           newPath
           setSourceRGBA 0 0 1 0.5
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

