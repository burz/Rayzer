module Base.Image
( Image
, saveToFile
) where

import Math.Vector
import Base.Material
import Base.Scene
import Base.Camera

import Codec.Picture hiding (Pixel, Image)
import Data.Word

type Pixel = PixelYCbCr8
type Image = DynamicImage

createWord8 :: Double -> Word8
createWord8 d = fromInteger $ round $ 255 * clip d
    where clip v = if v < 0 then 0 else if v > 1 then 1 else v

createRGB8 :: Color -> PixelRGB8
createRGB8 c = extract3 c appl
    where appl x y z
            = PixelRGB8 (createWord8 x) (createWord8 y) (createWord8 z)

generate :: Scene -> Camera -> Int -> Int -> Image
generate s c w h = ImageRGB8 $ generateImage gen w h
    where gen i j = createRGB8 $ pixelColor s c w h i j

saveToFile :: Scene -> Camera -> Int -> Int -> FilePath -> Int -> IO ()
saveToFile s c w h f q = do
    let i = generate s c w h
    saveJpgImage q f i

