{-# LANGUAGE TypeSynonymInstances #-}

module Math.Matrix
( Matrix3
, Matrix4
, identity
, (*.)
) where

import Math.Vector

import Data.Vect.Double

type Matrix3 = Mat3
type Matrix4 = Mat4

class Identity a where
    identity :: a

instance Identity Matrix3 where
    identity = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)

instance Identity Matrix4 where
    identity = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)

