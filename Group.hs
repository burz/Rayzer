{-# LANGUAGE TemplateHaskell #-}

module Group
( Group
, _End
, _Parent
) where

import Math.Ray
import Intersection
import Object

import Data.Matrix
import Control.Lens

data Group = End Object | Parent [Group] (Matrix Double)

makePrisms ''Group

intersectClosest :: Ray -> Double -> Group -> Maybe Intersection -> Maybe Intersection
intersectClosest r d g Nothing  = intersect g d r
intersectClosest r d g (Just i) = let mi = intersect g d r in case mi of
    Nothing -> Just i
    Just i' ->  if i' ^. distance > i ^. distance then Just i else Just i'

instance Intersectable Group where
    intersect (End o) d r = intersect o d r
    intersect (Parent gs m) d r = let r' = r -- TODO transform by m
        in foldr (intersectClosest r' d) Nothing gs

