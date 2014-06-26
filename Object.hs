{-# LANGUAGE TemplateHaskell #-}

module Object
( Object
, object
, obj
) where

import Math.Sphere
import Math.Intersection

import Control.Lens

data Object' a = Object { _obj :: a }

makeLenses ''Object'

instance Intersectable a => Intersectable (Object' a) where
    intersect r o = intersect r $ o ^. obj

data ObjectType =
      Sphr Sphere

instance Intersectable ObjectType where
    intersect r (Sphr s) = intersect r s

type Object = Object' ObjectType

object :: ObjectType -> Object
object o = Object { _obj = o
                  }

