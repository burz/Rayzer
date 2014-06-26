{-# LANGUAGE TemplateHaskell #-}

module Camera
( Camera(..)
, rayThroughPixel
) where

import Math.Vector
import Math.Ray

import Control.Lens

data Camera = Camera { _position      :: Vector
                     , _forward       :: Vector
                     , _up            :: Vector
                     , _heightAngle   :: Double
                     , _aspectRatio   :: Double
                     , _focus         :: Double
                     }

makeLenses ''Camera

right :: Camera -> Vector
right c = crossProduct (c ^. forward) (c ^. up)
 
rayThroughPixel :: Camera -> Int -> Int -> Int -> Int -> Ray
rayThroughPixel c i j w h =
    let pos = c ^. position in
    let p = addVector pos $ multiplyScalar (c ^. forward) $ c ^. focus in
    let t = tan $ c ^. heightAngle in
    let px = multiplyScalar (right c) $ t * c ^. aspectRatio * c ^. focus in
    let py = multiplyScalar (c ^. up) $ t * c ^. focus in
    let pc = subtractVector p $ subtractVector px py in
    let px' = multiplyScalar px $ 2 * fromIntegral i / fromIntegral w in
    let py' = multiplyScalar py $ 2 * fromIntegral j / fromIntegral h
    in Ray pos . addVector pc . addVector px' $ subtractVector py' pos
 
