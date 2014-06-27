{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Math.Vector
( Vector2(..)
, Vector3(..)
, Unit2(..)
, Unit3(..)
, vector2
, vector3
, origin
, unit
, fromUnit
, len
, (&.)
, (*&)
, (&^)
, reflect'
) where

import Data.Vect.Double
import Data.Vect.Double.Instances

type Vector2 = Vec2
type Vector3 = Vec3

type Unit2 = Normal2
type Unit3 = Normal3

vector2 :: Double -> Double -> Vector2
vector2 = Vec2

vector3 :: Double -> Double -> Double -> Vector3
vector3 x y z = mkVec3 (x, y, z)

origin = vector3 0 0 0

class Unit a b where
    unit     :: b -> a
    fromUnit :: a -> b

instance Unit Unit2 Vector2 where
    unit     = mkNormal
    fromUnit = fromNormal

instance Unit Unit3 Vector3 where
    unit     = mkNormal
    fromUnit = fromNormal

