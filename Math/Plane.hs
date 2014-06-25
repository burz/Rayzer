module Math.Plane
( Plane(..)
) where

import Math.Vector

data Plane =
      PointPlane Vector Vector Vector
    | DistancePlane Vector Double

