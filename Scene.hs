{-# LANGUAGE TemplateHaskell #-}

module Scene
( Scene(..)
, graph
, camera
, backgroundColor
) where

import Math.Vector
import Group
import Camera

import Control.Lens

data Scene = Scene { _graph           :: Group
                   , _camera          :: Camera
                   , _backgroundColor :: Vector
                   }

makeLenses ''Scene

