module Math.Ray
( Ray(..)
, throughPoint
) where

import Math.Vector

import Prelude hiding (subtract)

data Ray = Ray Vector Vector

throughPoint :: Vector -> Vector -> Ray
throughPoint s p = let d = unit $ subtract p s in Ray s d

