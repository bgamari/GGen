module GGen.Render (renderPath, renderPathsToSVG) where

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
        where (rminX, rminY, _) = rmin
              (rmaxX, rmaxY, _) = rmax
              (rw, rh) = (rmaxX-rminX, rmaxY-rminY)
              viewRatio = w/h
              regionRatio = rw/rh
              (cx, cy, _) = rmin + (rmax - rmin) ^/ 2
              widthConstrained = viewRatio < regionRatio
              s = if widthConstrained then 1/rw
                                      else 1/rh

renderPath :: LineSegPath -> (Point,Point) -> Render ()
renderPath path (rmin,rmax) =
        do setSourceRGB 0 0 0
           setLineWidth 1e-1
           newPath
           mapM_ drawSegment $ filter zClipped path
           stroke
        where (_,_,maxZ) = rmax
              (_,_,minZ) = rmin

              depthToAlpha z = 1

              drawSegment :: LineSeg -> Render ()
              drawSegment (LineSeg u@(ux,uy,uz) v@(vx,vy,vz)) =
                      do -- This is only correct for lines that don't travel far in Z
                         setSourceRGBA 0 0 0 $ depthToAlpha ((uz-minZ) / (maxZ-minZ))
                         lineTo ux uy
                         lineTo vx vy

              zClipped (LineSeg (_,_,az) (_,_,bz))
                       | az > maxZ && bz > maxZ  = False
                       | az < minZ && bz < minZ  = False
                       | otherwise               = True

renderPathsToSVG :: FilePath -> (Double,Double) -> (Point,Point) -> [LineSegPath] -> IO ()
renderPathsToSVG filename (w,h) region paths =
        withSVGSurface filename w h (flip renderWith $ do rescaleForRegion (w,h) region
                                                          mapM_ (\path->renderPath path region) paths
                                                          showPage)

