{-# LANGUAGE TemplateHaskell #-}

module Base.Material
( Color(..)
, Material
, material
, defaultMaterial
, ambient
, diffuse
, specular
, specularFallOff
) where

import Math.Vector

import Control.Lens

type Color = Vector3

data Material = Material { _ambient         :: Color
                         , _diffuse         :: Color
                         , _specular        :: Color
                         , _specularFallOff :: Double
                         }

material :: Color -> Color -> Color -> Double -> Material
material a d s f = Material { _ambient         = a
                            , _diffuse         = d
                            , _specular        = s
                            , _specularFallOff = f
                            }

defaultMaterial :: Material
defaultMaterial = let v0 = vector3 0 0 0 in
    let v1 = vector3 1 1 1 in material v1 v0 v0 0

makeLenses ''Material

