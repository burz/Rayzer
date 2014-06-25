module Math.Ray
( Ray(..)
, throughPoint
) where

import Math.Vector

data Ray = Ray Vector Vector

throughPoint :: Vector -> Vector -> Ray
throughPoint s p = let d = unit $ subtractVector p s
    in Ray s d

