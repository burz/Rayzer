module Light
( Light(..)
, intensity
, position
) where

import Math.Vector

import Prelude hiding (subtract)

data Light =
      Directional Vector Vector
    | Point Vector Vector Vector
    | Spot Vector Vector Vector Vector Double Double

calculateDenominator :: Vector -> Vector -> Vector -> Double
calculateDenominator p (Vector kc kl kq) v =
    let d = magnitude $ subtract v p in 1 / (kc + kl * d + kq * d * d)

intensity :: Light -> Vector -> Vector
intensity (Directional i _) _ = i
intensity (Point i p k) v = let d = calculateDenominator p k v in multiplyS i d
intensity (Spot i p k d g a) v = let q = calculateDenominator p k v in
    let dl = dotProduct d . unit $ subtract v p in
    if dl > cos g
        then multiplyS i $ q * dl ** a
        else Vector 0 0 0

position :: Light -> Vector
position (Directional _ p) = p
position (Point _ p _) = p
position (Spot _ p _ _ _ _) = p
    
