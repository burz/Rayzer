{-# LANGUAGE TemplateHaskell #-}

module Object
( Object(..)
, object
) where

import Math.Sphere

import Control.Lens

data Object' a = Object { _object :: a }

data ObjectType =
      Sphere Sphere

type Object = Object' ObjectType

makeLenses ''Object'

