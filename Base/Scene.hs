{-# LANGUAGE TemplateHaskell #-}

module Base.Scene
( Scene(..)
, scene
, group
, backgroundColor
, lights
, ambientLight
, intersect
, lightContribution
) where

import Math.Vector
import Base.Material
import Base.Group
import Base.Intersection
import Base.Light

import Control.Lens

data Scene = Scene { _group           :: Group
                   , _backgroundColor :: Color
                   , _lights          :: [Light]
                   , _ambientLight    :: Color
                   }

scene :: Group -> Color -> [Light] -> Color -> Scene
scene g b l a = Scene { _group           = g
                      , _backgroundColor = b
                      , _lights          = l
                      , _ambientLight    = a
                      }

makeLenses ''Scene

instance Intersectable Scene where
    intersect s = intersect (s ^. group)

lightContribution :: Scene -> Intersection -> Vector3 -> Color
lightContribution s i v
    = lightContributions i v (s ^. ambientLight) (s ^. lights)

