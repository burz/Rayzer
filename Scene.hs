{-# LANGUAGE TemplateHaskell #-}

module Scene
( Scene(..)
, group
, backgroundColor
, lights
, ambientLight
, lightContribution
) where

import Math.Vector
import Material
import Group
import Camera
import Intersection
import Light

import Control.Lens

data Scene = Scene { _group           :: Group
                   , _backgroundColor :: Color
                   , _lights          :: [Light]
                   , _ambientLight    :: Color
                   }

makeLenses ''Scene

lightContribution :: Scene -> Intersection -> Vector -> Color
lightContribution s i v
    = lightContributions i v (s ^. ambientLight) (s ^. lights)

instance Intersectable Scene where
    intersect s = intersect (s ^. group)

