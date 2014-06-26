module Light
( Light(..)
, lightContributions
) where

import Math.Vector
import Math.Ray
import Material
import Intersection

import Control.Lens

import Prelude hiding (subtract)

type Attenuation = Vector

data Light =
      Directional Color Vector
    | Point Color Vector Attenuation
    | Spot Color Vector Attenuation Vector Double Double

calculateDenominator :: Vector -> Attenuation -> Vector -> Double
calculateDenominator p (Vector kc kl kq) v =
    let d = magnitude $ subtract v p in 1 / (kc + kl * d + kq * d * d)

intensity :: Light -> Vector -> Color
intensity (Directional i _) _ = i
intensity (Point i p k) v = let d = calculateDenominator p k v in multiplyS i d
intensity (Spot i p k d g a) v = let q = calculateDenominator p k v in
    let dl = dotProduct d . unit $ subtract v p in
    if dl > cos g
        then multiplyS i $ q * dl ** a
        else Vector 0 0 0

direction :: Light -> Vector -> Vector
direction (Directional _ d) _ = d
direction (Point _ p _) v = unit $ subtract p v
direction (Spot _ p _ _ _ _) v = unit $ subtract p v

diffuse' :: Vector -> Vector -> Vector -> Color -> Color
diffuse' k n l i = multiply k . multiplyS i $ dotProduct n l

specular' :: Vector -> Vector -> Vector -> Color -> Double -> Color
specular' k v r i n = multiply k . multiplyS i $ dotProduct v r ** n

lightContribution :: Intersection -> Vector -> Light -> Color
lightContribution i v l = let ity = intensity l v in
    let dir = direction l v in let n = i ^. normal in
    let d = diffuse' (i ^. matrl . diffuse) n dir ity in
    let refl = reflection dir n in
    let s = specular' (i ^. matrl . specular) v refl ity (i ^. matrl . specularFallOff)
    in add d s

lightContributions :: Intersection -> Vector -> Color -> [Light] -> Color
lightContributions i v a ls = foldr contrib base ls
    where contrib l c = add c $ lightContribution i v l
          base = multiply a $ i ^. matrl . ambient

