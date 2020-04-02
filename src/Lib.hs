module Lib
    ( someFunc
    ) where

-- import Control.Monad.ST
import Codec.Picture
-- import Codec.Picture.Types
import System.Random
import Graphics.Rasterific
import Graphics.Rasterific.Texture

someFunc :: IO ()
someFunc = do
  g <- newStdGen
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
randomColor g0 = (PixelRGBA8 red green blue op, g4)
  where
    (red, g1) = randomR (0, 255) g0
    (green, g2) = randomR (0, 255) g1
    (blue, g3) = randomR (0, 255) g2
    (op, g4) = randomR (0, 255) g3

randomCoord :: RandomGen g => g -> (Point, g)
randomCoord g0 = (V2 x y, g2)
  where
    (x, g1) = randomR (0, 512) g0
    (y, g2) = randomR (0, 512) g1

addRandomTriangle :: RandomGen g => Drawing PixelRGBA8 () -> g -> (Drawing PixelRGBA8 (), g)
addRandomTriangle draw g0 = (addPolygon [coord1, coord2, coord3] color draw, g4)
  where
    (coord1, g1) = randomCoord g0
    (coord2, g2) = randomCoord g1
    (coord3, g3) = randomCoord g2
    (color, g4) = randomColor g3

