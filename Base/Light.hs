module Base.Light
( Light(..)
, lightContributions
) where

import Math.Vector
import Math.Ray
import Base.Material
import Base.Intersection

import Control.Lens

data Attenuation = Atten Double Double Double

data Light =
      Directional Color Unit3
    | Point Color Vector3 Attenuation
    | Spot Color Vector3 Attenuation Unit3 Double Double

calculateDenominator :: Vector3 -> Attenuation -> Vector3 -> Double
calculateDenominator p (Atten kc kl kq) v =
    let d = len $ v - p in 1 / (kc + kl * d + kq * d * d)

intensity :: Light -> Vector3 -> Color
intensity (Directional i _) _ = i
intensity (Point i p k) v = calculateDenominator p k v *& i
intensity (Spot i p k d g a) v = let q = calculateDenominator p k v in
    let dl = d &. unit (v - p) in
    if dl > cos g
        then q *& dl ** a *& i
        else vector3 0 0 0

direction :: Light -> Vector3 -> Unit3
direction (Directional _ d) _ = d
direction (Point _ p _) v = unit $ p - v
direction (Spot _ p _ _ _ _) v = unit $ p - v

diffuse' :: Color -> Unit3 -> Unit3 -> Color -> Color
diffuse' k n l i = k * ((n &. l) *& i)

specular' :: Color -> Unit3 -> Unit3 -> Color -> Double -> Color
specular' k v r i n = k * (((v &. r) ** n) *& i)

lightContribution :: Intersection -> Vector3 -> Light -> Color
lightContribution i v l = let ity = intensity l v in
    let dir = direction l v in let n = i ^. normal in
    let d = diffuse' (i ^. matrl . diffuse) n dir ity in
    let refl = reflect' dir n in
    let spec = i ^. matrl . specular in
    let specFall = i ^. matrl . specularFallOff in
    let s = specular' spec (unit v) refl ity specFall in d + s

lightContributions :: Intersection -> Vector3 -> Color -> [Light] -> Color
lightContributions i v a ls = foldr contrib base ls
    where contrib l c = c + lightContribution i v l
          base = a * i ^. matrl . ambient

