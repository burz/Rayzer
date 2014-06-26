{-# LANGUAGE TemplateHaskell #-}

module Math.Intersection
( Intersection
, intersection
, Intersectable(..)
) where

import Math.Vector
import Math.Ray

import Control.Lens

data Intersection = Intersection { _distance :: Double
                                 , _position :: Vector
                                 , _normal   :: Vector
                                 }

intersection :: Double -> Vector -> Vector -> Intersection
intersection d p n = Intersection { _distance = d
                                  , _position = p
                                  , _normal = n
                                  }

makeLenses ''Intersection

class Intersectable a where
    intersect :: Ray -> a -> Double -> Maybe Intersection

