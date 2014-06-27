{-# LANGUAGE TemplateHaskell #-}

module Scene
( Scene(..)
, group
, backgroundColor
, lights
, ambientLight
, intersect
, lightContribution
) where

import Math.Vector
import Material
import Group
import Intersection
import Light

import Control.Lens

data Scene = Scene { _group           :: Group
                   , _backgroundColor :: Color
                   , _lights          :: [Light]
                   , _ambientLight    :: Color
                   }

makeLenses ''Scene

instance Intersectable Scene where
    intersect s = intersect (s ^. group)

lightContribution :: Scene -> Intersection -> Vector3 -> Color
lightContribution s i v
    = lightContributions i v (s ^. ambientLight) (s ^. lights)

