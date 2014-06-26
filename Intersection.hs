{-# LANGUAGE TemplateHaskell #-}

module Intersection
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
import Material

import Control.Lens

data Intersection = Intersection { _distance :: Double
                                 , _position :: Vector
                                 , _normal   :: Vector
                                 , _matrl    :: Material
                                 }

intersection :: Double -> Vector -> Vector -> Intersection
intersection d p n = Intersection { _distance = d
                                  , _position = p
                                  , _normal   = n
                                  , _matrl    = defaultMaterial
                                  }

makeLenses ''Intersection

class Intersectable a where
    intersect :: Ray -> a -> Double -> Maybe Intersection

