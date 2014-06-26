{-# LANGUAGE TemplateHaskell #-}

module Scene
( Scene(..)
, graph
, camera
, backgroundColor
, lights
) where

import Math.Vector
import Group
import Camera
import Light

import Control.Lens

data Scene = Scene { _graph           :: Group
                   , _camera          :: Camera
                   , _backgroundColor :: Vector
                   , _lights          :: [Light]
                   }

makeLenses ''Scene

