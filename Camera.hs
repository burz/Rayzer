{-# LANGUAGE TemplateHaskell #-}

module Camera
( Camera
, camera
, position
, forward
, up
, heightAngle
, aspectRatio
, focus
, depth
, rayThroughPixel
) where

import Math.Vector
import Math.Ray

import Control.Lens
import Prelude hiding (subtract)

data Camera = Camera { _position    :: Vector
                     , _forward     :: Vector
                     , _up          :: Vector
                     , _heightAngle :: Double
                     , _aspectRatio :: Double
                     , _focus       :: Double
                     , _depth       :: Double
                     }

camera :: Vector -> Vector -> Vector -> Double -> Double -> Double -> Double -> Camera
camera p f u h a fc d = Camera { _position    = p
                               , _forward     = f
                               , _up          = u
                               , _heightAngle = h
                               , _aspectRatio = a
                               , _focus       = fc
                               , _depth       = d
                               }

makeLenses ''Camera

right :: Camera -> Vector
right c = crossProduct (c ^. forward) (c ^. up)
 
rayThroughPixel :: Camera -> Int -> Int -> Int -> Int -> Ray
rayThroughPixel c i j w h = let pos = c ^. position in
    let p = add pos $ multiplyS (c ^. forward) $ c ^. focus in
    let t = tan $ c ^. heightAngle in
    let px = multiplyS (right c) $ t * c ^. aspectRatio * c ^. focus in
    let py = multiplyS (c ^. up) $ t * c ^. focus in
    let pc = subtract p $ subtract px py in
    let px' = multiplyS px $ 2 * fromIntegral i / fromIntegral w in
    let py' = multiplyS py $ 2 * fromIntegral j / fromIntegral h
    in Ray pos . add pc . add px' $ subtract py' pos
 
