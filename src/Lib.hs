module Lib
    ( someFunc
    ) where

import Control.Monad.State.Lazy
import Codec.Picture
import System.Random
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture.RGBA8

someFunc :: IO ()
someFunc = do
  g <- newStdGen
  imageLoad <- readImage "input.png"
  case imageLoad of
    Left error -> putStrLn error
    Right image -> putStrLn "Input image is loaded"

  writePng "output.png" $ finalImage g

finalImage :: RandomGen g => g -> Image PixelRGBA8
finalImage g = renderDrawing 512 512 white final
  where
    white = PixelRGBA8 255 255 255 255
    (final, _) = addNRandomTriangles mempty g 2500

addNRandomTriangles :: RandomGen g => Drawing PixelRGBA8 () -> g -> Int -> (Drawing PixelRGBA8 (), g)
addNRandomTriangles draw g 0 = (draw, g)
addNRandomTriangles draw g n = addNRandomTriangles newDraw newG (n-1)
  where
    (newDraw, newG) = addRandomTriangle draw g

addPolygon :: Pixel a => [Point] -> a -> Drawing a () -> Drawing a ()
addPolygon points color drawing = withTexture (uniformTexture color) $ do
  fill $ polygon points
  drawing

randomColor :: RandomGen g => g -> (PixelRGBA8, g)
randomColor = runState (liftM4 PixelRGBA8 r r r r)
  where
    r = state $ randomR (0, 255)

randomCoord :: RandomGen g => g -> (Point, g)
randomCoord = runState (liftM2 V2 r r)
  where
    r = state $ randomR (0, 512)

addRandomTriangle :: RandomGen g => Drawing PixelRGBA8 () -> g -> (Drawing PixelRGBA8 (), g)
addRandomTriangle draw g0 = (addPolygon [coord1, coord2, coord3] color draw, g4)
  where
    (coord1, g1) = randomCoord g0
    (coord2, g2) = randomCoord g1
    (coord3, g3) = randomCoord g2
    (color, g4) = randomColor g3

