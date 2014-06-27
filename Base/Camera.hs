{-# LANGUAGE TemplateHaskell #-}

module Base.Camera
( Camera
, camera
, defaultCamera
, position
, forward
, up
, heightAngle
, aspectRatio
, focus
, depth
, pixelColor
) where

import Math.Vector
import Math.Ray
import Base.Material
import Base.Scene

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

defaultCamera :: Camera
defaultCamera = camera origin (unit3 0 0 1) (unit3 0 1 0) 0.5 0.5 0.1 100

makeLenses ''Camera

right :: Camera -> Unit3
right c = (c ^. forward) &^ (c ^. up)
 
rayThroughPixel :: Camera -> Int -> Int -> Int -> Int -> Ray
rayThroughPixel c w h i j = let pos = c ^. position in
    let p = pos + (c ^. focus) *& (fromUnit $ c ^. forward) in
    let t = tan $ c ^. heightAngle in
    let px = (t * c ^. aspectRatio * c ^. focus) *& fromUnit (right c) in
    let py = (t * c ^. focus) *& fromUnit (c ^. up) in
    let pc = p - px - py in
    let px' = (2 * fromIntegral i / fromIntegral w) *& px in
    let py' = (2 * fromIntegral j / fromIntegral h) *& py
    in Ray pos . unit $ pc + px' + py' - pos
 
pixelColor :: Scene -> Camera -> Int -> Int -> Int -> Int -> Color
pixelColor s c w h i j = let r = rayThroughPixel c i j w h in
    let mi = intersect s (c ^. depth) r in case mi of
        Nothing -> s ^. backgroundColor
        Just i  -> lightContribution s i (c ^. position)

