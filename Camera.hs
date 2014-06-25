module Camera
( Camera(..)
, rayThroughPixel
) where

import Math.Vector
import Math.Ray

data Camera = Camera { position      :: Vector
                     , forward       :: Vector
                     , up            :: Vector
                     , heightAngle    :: Double
                     , aspectRatio   :: Double
                     , focus         :: Double
                     }

right :: Camera -> Vector
right c = crossProduct (forward c) (up c)
 
rayThroughPixel :: Camera -> Int -> Int -> Int -> Int -> Ray
rayThroughPixel c i j w h =
    let pos = position c in
    let p = addVector pos $ multiplyScalar (forward c) $ focus c in
    let t = tan $ heightAngle c in
    let px = multiplyScalar (right c) $ t * aspectRatio c * focus c in
    let py = multiplyScalar (up c) $ t * focus c in
    let pc = subtractVector p $ subtractVector px py in
    let px' = multiplyScalar px $ 2 * fromIntegral i / fromIntegral w in
    let py' = multiplyScalar py $ 2 * fromIntegral j / fromIntegral h
    in Ray pos . addVector pc . addVector px' $ subtractVector py' pos
 
