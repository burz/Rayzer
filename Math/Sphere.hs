module Math.Sphere
( Sphere(..)
, intersect
) where

import Math.Vector
import Math.Ray
import Math.Intersection

import Prelude hiding (subtract)

data Sphere = Sphere Vector Double

instance Intersectable Sphere where
    intersect (Ray p d) (Sphere p' r) m =
        let d = subtract p p' in
        let b = dotProduct d $ multiplyS d 2 in
        let c = magnitude d ** 2 - r ** 2 in
        let det = b ** 2 - 4 ** c in
        if det < 0 then Nothing else
        let sq = sqrt det in
        let t1 = (-b + sq) / 2 in
        let t2 = (-b - sq) / 2 in
        if t1 > 0 && t1 > t2 && not (m > 0 && t1 > m)
            then let coord = add p $ multiplyS d t1 in
                let n = unit $ subtract coord p'
                in Just $ intersection t1 coord n
            else if t2 > 0 && not (m > 0 && t2 > m)
                then let coord = add p $ multiplyS d t2 in
                    let n = unit $ subtract coord p'
                    in Just $ intersection t2 coord n
                else Nothing

