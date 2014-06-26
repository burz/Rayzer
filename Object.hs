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
    intersect o m r = (& matrl .~ (o ^. objMatrl)) <$> intersect (o ^. obj) m r

data ObjectType =
      Sphr Sphere

instance Intersectable ObjectType where
    intersect (Sphr s) = intersect s

type Object = Object' ObjectType

object :: ObjectType -> Material -> Object
object o m = Object { _obj      = o
                    , _objMatrl = m
                    }
