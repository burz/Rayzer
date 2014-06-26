{-# LANGUAGE TemplateHaskell #-}

module Object
( Object
, object
, obj
, objMatrl
) where

import Math.Sphere
import Material
import Intersection

import Control.Lens
import Control.Applicative ((<$>))

data Object' a = Object { _obj      :: a
                        , _objMatrl :: Material
                        }

makeLenses ''Object'

instance Intersectable a => Intersectable (Object' a) where
    intersect r o m = (& matrl .~ (o ^. objMatrl)) <$> intersect r (o ^. obj) m

data ObjectType =
      Sphr Sphere

instance Intersectable ObjectType where
    intersect r (Sphr s) = intersect r s

type Object = Object' ObjectType

object :: ObjectType -> Material -> Object
object o m = Object { _obj      = o
                    , _objMatrl = m
                    }
