module Image
( Image
) where

import qualified Codec.Picture.Types as CPT
import Codec.Picture.Jpg

import Data.ByteString.Lazy.Internal

type Image = CPT.Image CPT.PixelYCbCr8

encode :: Image -> ByteString
encode = encodeJpeg

