module Math.Vector
( Vector(..)
, magnitude
, dotProduct
, crossProduct
) where

data Vector = Vector Double Double Double

magnitude :: Vector -> Double
magnitude (Vector x y z) = sqrt $ x * x + y * y + z * z

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector x y z) (Vector x' y' z')
    = x * x' + y * y' + z * z'

crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector x y z) (Vector x' y' z')
    = Vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

