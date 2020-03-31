{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( someFunc
    ) where

import Control.Monad.ST
import Codec.Picture
import Codec.Picture.Drawing
import Codec.Picture.Geometry
import Codec.Picture.Types

someFunc :: IO ()
someFunc = writePng "output.png" $ addPolygon blankImage

addPolygon :: Image PixelRGBA8 -> Image PixelRGBA8
addPolygon img = runST $ do
  mimg <- thawImage img
  fillPolygon mimg ((closed . clockwise) [(1,1), (150, 300), (300, 150)]) (PixelRGBA8 100 100 100 255)
  unsafeFreezeImage mimg


blankImage :: Image PixelRGBA8
blankImage =
  generateImage (\_ _ -> PixelRGBA8 255 255 255 255) 512 512

