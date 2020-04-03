module Lib
    ( someFunc
    ) where

import Control.Monad.State.Lazy
import Codec.Picture
import System.Random
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture.RGBA8
import Codec.Picture.Types
-- import Conversion

imageSize :: (Int, Int)
imageSize = (512, 512)

someFunc :: IO ()
someFunc = do
  g <- newStdGen
  imageLoad <- readImage "input.png"
  case imageLoad of
    Left err -> putStrLn err
    Right image -> do
      let outputImg = finalImage g
          inputImg = fromDynamicImage image

      putStrLn $ show $ imageDiff inputImg outputImg
      writePng "output.png" $ outputImg

finalImage :: RandomGen g => g -> Image PixelRGBA8
finalImage g = renderDrawing (fst imageSize) (snd imageSize) white final
  where
    white = PixelRGBA8 255 255 255 255
    (final, _) = addNRandomTriangles mempty g 500

pixelDiff :: PixelRGBA8 -> PixelRGBA8 -> Integer
pixelDiff px1 px2 = (abs (r1-r2)) + (abs (g1-g2)) + (abs (b1-b2))
  where
    [r1, g1, b1, r2, g2, b2] = toInteger <$> [r1_, g1_, b1_, r2_, g2_, b2_]
    (PixelRGBA8 r1_ g1_ b1_ _) = px1
    (PixelRGBA8 r2_ g2_ b2_ _) = px2

imageDiff :: Image PixelRGBA8 -> Image PixelRGBA8 -> Integer
imageDiff img1 img2 =
  let go x y n
        | x >= (fst imageSize) = go 0 (y+1) n
        | y >= (snd imageSize) = n
        | otherwise =
          go (x+1) y (n + pixelDiff (pixelAt img1 x y) (pixelAt img2 x y))
  in go 0 0 0

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
randomCoord = runState (liftM2 V2 r1 r2)
  where
    r1 = state $ randomR (0, realToFrac $ fst imageSize)
    r2 = state $ randomR (0, realToFrac $ snd imageSize)

addRandomTriangle :: RandomGen g => Drawing PixelRGBA8 () -> g -> (Drawing PixelRGBA8 (), g)
addRandomTriangle draw g0 = (addPolygon [coord1, coord2, coord3] color draw, g4)
  where
    (coord1, g1) = randomCoord g0
    (coord2, g2) = randomCoord g1
    (coord3, g3) = randomCoord g2
    (color, g4) = randomColor g3

