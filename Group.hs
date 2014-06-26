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
intersectClosest r d g Nothing  = intersect r g d
intersectClosest r d g (Just i) = let mi = intersect r g d in case mi of
    Nothing -> Just i
    Just i' ->  if i' ^. distance > i ^. distance then Just i else Just i'

instance Intersectable Group where
    intersect r (End o) d = intersect r o d
    intersect r (Parent gs m) d = let r' = r -- TODO transform by m
        in foldr (intersectClosest r' d) Nothing gs

