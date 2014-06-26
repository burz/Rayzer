module Math.Vector
( Vector(..)
, origin
, magnitude
, unit
, addS
, multiplyS
, add
, subtract
, multiply
, dotProduct
, crossProduct
, reflection
) where

import Prelude hiding (subtract)

data Vector = Vector Double Double Double deriving Show

origin = Vector 0 0 0

magnitude :: Vector -> Double
magnitude (Vector x y z) = sqrt $ x * x + y * y + z * z

unit :: Vector -> Vector
unit v = let l = magnitude v in multiplyS v $ 1 / l

addS :: Vector -> Double -> Vector
addS (Vector x y z) d = Vector (x + d) (y + d) (z + d)

multiplyS :: Vector -> Double -> Vector
multiplyS (Vector x y z) d = Vector (x * d) (y * d) (z * d)

add :: Vector -> Vector -> Vector
add (Vector x y z) (Vector x' y' z')
    = Vector (x + x') (y + y') (z + z')

subtract :: Vector -> Vector -> Vector
subtract (Vector x y z) (Vector x' y' z')
    = Vector (x - x') (y - y') (z - z')

multiply :: Vector -> Vector -> Vector
multiply (Vector x y z) (Vector x' y' z')
    = Vector (x * x') (y * y') (z * z')

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector x y z) (Vector x' y' z')
    = x * x' + y * y' + z * z'

crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector x y z) (Vector x' y' z')
    = Vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

reflection :: Vector -> Vector -> Vector
reflection v n = subtract v . multiplyS n $ 2 * dotProduct v n

