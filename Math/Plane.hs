module Math.Plane
( Plane(..)
) where

import Math.Vector

data Plane =
      PointPlane Vector3 Vector3 Vector3
    | DistancePlane Vector3 Double

