module SphereScene
( save
) where

import Math.Vector
import Math.Matrix
import Math.Sphere
import Base.Material
import Base.Group
import Base.Scene hiding (backgroundColor, ambientLight)
import Base.Camera
import Base.Image

backgroundColor :: Color
backgroundColor = vector3 0 0 0

ambientLight :: Color
ambientLight = vector3 1 1 1

sphereScene :: Scene
sphereScene = scene (Parent [] identity) backgroundColor [] ambientLight

save :: Int -> Int -> Int -> IO ()
save w h q = saveToFile sphereScene defaultCamera w h "SphereScene.jpg" q

