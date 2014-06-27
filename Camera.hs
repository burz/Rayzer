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

data Camera = Camera { _position    :: Vector3
                     , _forward     :: Unit3
                     , _up          :: Unit3
                     , _heightAngle :: Double
                     , _aspectRatio :: Double
                     , _focus       :: Double
                     , _depth       :: Double
                     }

camera :: Vector3 -> Unit3 -> Unit3 -> Double -> Double -> Double -> Double -> Camera
camera p f u h a fc d = Camera { _position    = p
                               , _forward     = f
                               , _up          = u
                               , _heightAngle = h
                               , _aspectRatio = a
                               , _focus       = fc
                               , _depth       = d
                               }

makeLenses ''Camera

right :: Camera -> Unit3
right c = (c ^. forward) &^ (c ^. up)
 
rayThroughPixel :: Camera -> Int -> Int -> Int -> Int -> Ray
rayThroughPixel c i j w h = let pos = c ^. position in
    let p = pos + (c ^. focus) *& (fromUnit $ c ^. forward) in
    let t = tan $ c ^. heightAngle in
    let px = (t * c ^. aspectRatio * c ^. focus) *& fromUnit (right c) in
    let py = (t * c ^. focus) *& fromUnit (c ^. up) in
    let pc = p - px - py in
    let px' = (2 * fromIntegral i / fromIntegral w) *& px in
    let py' = (2 * fromIntegral j / fromIntegral h) *& py
    in Ray pos . unit $ pc + px' + py' - pos
 
