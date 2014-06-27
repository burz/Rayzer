module Math.Ray
( Ray(..)
, throughPoint
) where

import Math.Vector

data Ray = Ray Vector3 Unit3

throughPoint :: Vector3 -> Vector3 -> Ray
throughPoint s p = let d = unit $ p - s in Ray s d

