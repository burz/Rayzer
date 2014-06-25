module Math.Vector
( Vector(..)
, origin
, magnitude
, unit
, addScalar
, multiplyScalar
, addVector
, subtractVector
, multiplyVector
, dotProduct
, crossProduct
) where

data Vector = Vector Double Double Double

origin = Vector 0 0 0

magnitude :: Vector -> Double
magnitude (Vector x y z) = sqrt $ x * x + y * y + z * z

unit :: Vector -> Vector
unit v = let l = magnitude v in multiplyScalar v $ 1 / l

addScalar :: Vector -> Double -> Vector
addScalar (Vector x y z) d = Vector (x + d) (y + d) (z + d)

multiplyScalar :: Vector -> Double -> Vector
multiplyScalar (Vector x y z) d = Vector (x * d) (y * d) (z * d)

addVector :: Vector -> Vector -> Vector
addVector (Vector x y z) (Vector x' y' z')
    = Vector (x + x') (y + y') (z + z')

subtractVector :: Vector -> Vector -> Vector
subtractVector (Vector x y z) (Vector x' y' z')
    = Vector (x - x') (y - y') (z - z')

multiplyVector :: Vector -> Vector -> Vector
multiplyVector (Vector x y z) (Vector x' y' z')
    = Vector (x * x') (y * y') (z * z')

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector x y z) (Vector x' y' z')
    = x * x' + y * y' + z * z'

crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector x y z) (Vector x' y' z')
    = Vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

