{-# LANGUAGE TemplateHaskell #-}

module Object
( Object
, object
, obj
) where

import Math.Sphere

import Control.Lens

data Object' a = Object { _obj :: a }

data ObjectType =
      Sphere Sphere

type Object = Object' ObjectType

object :: ObjectType -> Object
object o = Object { _obj = o
                  }

makeLenses ''Object'

