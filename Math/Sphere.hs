module Math.Sphere
( Sphere(..)
, intersect
) where

import Math.Vector
import Math.Ray
import Intersection

data Sphere = Sphere Vector3 Double

instance Intersectable Sphere where
    intersect (Sphere p' r) m (Ray p dir) =
        let d = p - p' in
        let b = d &. (2 *& fromUnit dir) in
        let c = len d ** 2 - r ** 2 in
        let det = b ** 2 - 4 ** c in
        if det < 0 then Nothing else
        let sq = sqrt det in
        let t1 = (-b + sq) / 2 in
        let t2 = (-b - sq) / 2 in
        if t1 > 0 && t1 > t2 && not (m > 0 && t1 > m)
            then let coord = p + t1 *& d in
                let n = unit $ coord - p'
                in Just $ intersection t1 coord n
            else if t2 > 0 && not (m > 0 && t2 > m)
                then let coord = p + t2 *& d in
                    let n = unit $ coord - p'
                    in Just $ intersection t2 coord n
                else Nothing

