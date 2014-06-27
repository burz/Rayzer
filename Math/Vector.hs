{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Math.Vector
( Vector2(..)
, Vector3(..)
, Unit2(..)
, Unit3(..)
, vector2
, vector3
, unit2
, unit3
, origin
, unit
, fromUnit
, len
, (&.)
, (*&)
, (&^)
, reflect'
, extract3
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
vector3 = Vec3

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

unit2 :: Double -> Double -> Unit2
unit2 x y = unit $ vector2 x y

unit3 :: Double -> Double -> Double -> Unit3
unit3 x y z = unit $ vector3 x y z

extract3 :: Vector3 -> (Double -> Double -> Double -> a) -> a
extract3 (Vec3 x y z) f = f x y z

