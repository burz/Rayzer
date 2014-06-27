{-# LANGUAGE TemplateHaskell #-}

module Base.Intersection
( Intersection
, intersection
, distance
, position
, normal
, matrl
, Intersectable(..)
) where

import Math.Vector
import Math.Ray
import Base.Material

import Control.Lens

data Intersection = Intersection { _distance :: Double
                                 , _position :: Vector3
                                 , _normal   :: Unit3
                                 , _matrl    :: Material
                                 }

intersection :: Double -> Vector3 -> Unit3 -> Intersection
intersection d p n = Intersection { _distance = d
                                  , _position = p
                                  , _normal   = n
                                  , _matrl    = defaultMaterial
                                  }

makeLenses ''Intersection

class Intersectable a where
    intersect :: a -> Double -> Ray -> Maybe Intersection

