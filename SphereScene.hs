module SphereScene
( save
) where

import Math.Vector
import Math.Matrix
import Math.Sphere
import Base.Material
import Base.Object
import Base.Group
import Base.Scene hiding (backgroundColor, ambientLight)
import Base.Camera
import Base.Image

backgroundColor = vector3 0 0 1

ambientLight = vector3 1 1 1

sphereLocation = vector3 0 0 100

sphereRadius = 50

sphere = Sphr $ Sphere sphereLocation sphereRadius

sphereColor = vector3 1 0 0

sphereMaterial = material sphereColor sphereColor sphereColor 0

sphereScene :: Scene
sphereScene = scene (end sphere sphereMaterial) backgroundColor [] ambientLight

save :: Int -> Int -> Int -> IO ()
save w h q = saveToFile sphereScene defaultCamera w h "SphereScene.jpg" q

