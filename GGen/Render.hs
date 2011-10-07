module GGen.Render ( renderPath
                   , renderPathsToSVG
                   , renderRegionToSVG
                   , renderPolygons
                   , renderOrientedPolygons
                   ) where

import Debug.Trace

import Graphics.Rendering.Cairo
import GGen.Geometry.Types
import Data.VectorSpace

type Vec2 = (Double, Double)
type Point2 = Vec2

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

-- | Render a line segment path
renderPath :: LineSegPath -> (Point,Point) -> Render ()
renderPath path (rmin,rmax) =
        do setSourceRGB 0 0 0
           newPath
           mapM_ drawSegment $ filter zClipped path
           stroke
        where (_,_,maxZ) = rmax
              (_,_,minZ) = rmin

              depthToAlpha z = 1

              drawSegment' :: LineSeg -> Render ()
              drawSegment' l@(LineSeg (_,_,z) _) =
                      do -- This is only correct for lines that don't travel far in Z
                         -- TODO: Fix
                         --setSourceRGBA 0 0 0 $ depthToAlpha ((u-minZ) / (maxZ-minZ))
                         drawSegment l

              zClipped (LineSeg (_,_,az) (_,_,bz))
                       | az > maxZ && bz > maxZ  = False
                       | az < minZ && bz < minZ  = False
                       | otherwise               = True

renderRegionToSVG :: FilePath -> (Double,Double) -> (Point,Point) -> Render () -> IO ()
renderRegionToSVG filename (w,h) region action =
        withSVGSurface filename w h (flip renderWith $ do rescaleForRegion (w,h) region
                                                          action
                                                          showPage)

renderPathsToSVG :: FilePath -> (Double,Double) -> (Point,Point) -> [LineSegPath] -> IO ()
renderPathsToSVG filename (w,h) region paths =
        renderRegionToSVG filename (w,h) region (mapM_ (\path->renderPath path region) paths)

-- | Draws a line segment
drawSegment :: LineSeg -> Render ()
drawSegment (LineSeg u@(ux,uy,uz) v@(vx,vy,vz)) =
        do lineTo ux uy
           lineTo vx vy

-- | Render polygons
renderPolygons :: [Polygon] -> Render ()
renderPolygons polys =
        do newPath
           setFillRule FillRuleEvenOdd
           setSourceRGBA 0 0 1 0.5
           mapM_ drawPolygon polys
           fill

           newPath
           setSourceRGBA 0 0 0 1
           mapM_ drawPolygon polys
           stroke

-- | Draw a polygon path
drawPolygon :: Polygon -> Render ()
drawPolygon poly = do moveTo x y
                      mapM_ (\(x,y,z) -> lineTo x y) poly
        where (x,y,_) = head poly

-- | Render polygons
renderOrientedPolygons :: [OrientedPolygon] -> Render ()
renderOrientedPolygons polys =
        do setFillRule FillRuleEvenOdd
           newPath
           setSourceRGBA 0 0 1 0.5
           mapM_ (drawPolygon . fst) polys
           fill

           newPath
           setSourceRGBA 1 0 0 1
           mapM_ (drawPolygon . fst) $ filter (\(_,fill) -> fill) polys
           stroke

           newPath
           setSourceRGBA 0 0 0 1
           mapM_ (drawPolygon . fst) $ filter (\(_,fill) -> not fill) polys
           stroke

