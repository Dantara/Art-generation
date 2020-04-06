module Lib
    ( someFunc
    ) where

import Control.Monad.State.Lazy
import Codec.Picture
import System.Random
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture.RGBA8
import Data.Sort
-- import Codec.Picture.Types
-- import Conversion

data Polygon = Polygon { polygonCoords :: [Point], polygonColor :: PixelRGBA8} deriving (Show)
type Gene = Polygon
type Chromosome = [Gene]
type Population = [Chromosome]

imageSize :: (Int, Int)
imageSize = (512, 512)

polygonVertices :: Int
polygonVertices = 6

chromosomeSize :: Int
chromosomeSize = 50

populationSize :: Int
populationSize = 16

parentsAmount :: Int
parentsAmount = 2

gaN :: Int
gaN = 320

someFunc :: IO ()
someFunc = do
  g <- newStdGen
  imageLoad <- readImage "input2.png"
  case imageLoad of
    Left err -> putStrLn err
    Right image -> do
      let inputImg = fromDynamicImage image
          outputImg = finalImage inputImg g

      putStr "Final fitness is: "
      putStrLn $ (show $ percentFitness $ imageDiff inputImg outputImg) ++ "%"
      writePng "output.png" $ outputImg

addNRandom :: RandomGen g => Int -> (g -> (a, g)) -> ([a], g) -> ([a], g)
addNRandom 0 _ x = x
addNRandom n f (ys, g) = addNRandom (n-1) f (y : ys, newG)
  where
    (y, newG) = f g

initPolygon :: RandomGen g => g -> (Polygon, g)
initPolygon g0 = (Polygon coords color, g2)
  where
    (coords, g1) = nRandomCoords polygonVertices g0
    (color, g2) = randomColor g1

initChromosome :: RandomGen g => g -> (Chromosome, g)
initChromosome g = addNRandom chromosomeSize initPolygon ([], g)

initPopulation :: RandomGen g => g -> (Population, g)
initPopulation g = addNRandom populationSize initChromosome ([], g)

chromosomeToDraw :: Chromosome -> Drawing PixelRGBA8 () -> Drawing PixelRGBA8 ()
chromosomeToDraw [] = id
chromosomeToDraw (p:ps) = chromosomeToDraw ps . addPolygon coords color
  where
    (Polygon coords color) = p

renderChromosome :: Chromosome -> Image PixelRGBA8
renderChromosome c = renderDrawing width height white draw
  where
    (width, height) = imageSize
    white = PixelRGBA8 255 255 255 255
    draw = chromosomeToDraw c mempty

pixelDiff :: PixelRGBA8 -> PixelRGBA8 -> Integer
pixelDiff px1 px2 = (abs (r1-r2)) + (abs (g1-g2)) + (abs (b1-b2))
  where
    [r1, g1, b1, r2, g2, b2] = toInteger <$> [r1_, g1_, b1_, r2_, g2_, b2_]
    (PixelRGBA8 r1_ g1_ b1_ _) = px1
    (PixelRGBA8 r2_ g2_ b2_ _) = px2

imageDiff :: Image PixelRGBA8 -> Image PixelRGBA8 -> Integer
imageDiff img1 img2 = sum $ do
  x <- [0..(x_max-1)]
  y <- [0..(y_max-1)]
  return $ pixelDiff (pixelAt img1 x y) (pixelAt img2 x y)
    where
      (x_max, y_max) = imageSize

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

nRandomCoords :: RandomGen g => Int -> g -> ([Point], g)
nRandomCoords n g = addNRandom n randomCoord ([], g)

mutateColorById :: RandomGen g => Int -> PixelRGBA8 -> g -> (PixelRGBA8, g)
mutateColorById n color g0
 | n == 1 = (PixelRGBA8 rand g b a, newG)
 | n == 2 = (PixelRGBA8 r rand b a, newG)
 | n == 3 = (PixelRGBA8 r g rand a, newG)
 | otherwise = (PixelRGBA8 r g b rand, newG)
   where
       (PixelRGBA8 r g b a) = color
       (rand, newG) = randomR (0, 255) g0

mutateColor :: RandomGen g => PixelRGBA8 -> g -> (PixelRGBA8, g)
mutateColor color g0 = mutateColorById index color g1
  where
    (index, g1) = randomR (1, 4) g0

mutateXById :: RandomGen g => Int -> (g -> (a, g)) -> [a] -> g -> ([a], g)
mutateXById _ _ [] g = ([], g)
mutateXById n f x@(_:ys) g
  | n == 0 = (newY:ys, newG)
  | otherwise = (concat (first, newY:newYs), newG)
    where
      (first, _:newYs) = splitAt (n-1) x
      (newY, newG) = f g

mutateCoords :: RandomGen g => [Point] -> g -> ([Point], g)
mutateCoords coords g0 = mutateXById index randomCoord coords g1
  where
    (index, g1) = randomR (0, length coords - 1) g0

mutateGene :: RandomGen g => Gene -> g -> (Gene, g)
mutateGene gene g0 =
  case index of
    0 -> (Polygon newCoords color, g2)
    otherwise -> (Polygon coords newColor, g3)
  where
      index :: Int
      (Polygon coords color) = gene
      (index, g1) = randomR (0, 1) g0
      (newCoords, g2) = mutateCoords coords g1
      (newColor, g3) = mutateColor color g1

crossover :: Chromosome -> Chromosome -> [Chromosome]
crossover parent1 parent2 = [child1, child2]
  where
    (f1, s1) = splitAt (length parent1 `div` 2) parent1
    (f2, s2) = splitAt (length parent2 `div` 2) parent2
    child1 = concat (f1, s2)
    child2 = concat (f2, s1)

chromosomeDiff :: Chromosome -> Image PixelRGBA8 -> (Chromosome, Integer)
chromosomeDiff c img = (c, imageDiff img newImg)
  where
    newImg = renderChromosome c

populationDiff :: Population -> Image PixelRGBA8 -> [(Chromosome, Integer)]
populationDiff cs img = map (\c -> chromosomeDiff c img) cs

getParents :: Int -> [(Chromosome, Integer)]  -> [Chromosome]
getParents n gs = map (\(c,_) -> c) parents
  where
    parents = take n $ sortBy (\(_,i1) (_,i2) -> compare i1 i2) gs

addToOffspring :: [Chromosome] -> [Chromosome] -> [Chromosome]
addToOffspring [] os = os
addToOffspring (p:ps) os = addToOffspring ps $ concat (os, newOs)
  where
    newOs = concat [(crossover p x) | x <- ps]

offspring :: [Chromosome] -> [Chromosome]
offspring cs = addToOffspring cs []

mutateChromosome :: RandomGen g => Chromosome -> g -> (Chromosome, g)
mutateChromosome c g = mutateXById index (mutateGene $ c !! index) c newG
  where
    (index, newG) = randomR (0, length c - 1) g

mutateChromosomes :: RandomGen g => [Chromosome] -> g -> [(Chromosome, g)]
mutateChromosomes [] _ = []
mutateChromosomes (c:cs) g = (newC, newG) : (mutateChromosomes cs newG)
  where
    (newC, newG) = mutateChromosome c g

mutatePopulation :: RandomGen g => Population -> g -> (Population, g)
mutatePopulation cs g = (newCs, last states)
  where
    (newCs, states) = unzip $ mutateChromosomes cs g

combinePopulation :: RandomGen g => [Chromosome] -> [Chromosome] -> g -> (Population, g)
combinePopulation parents children g = (p ++  newP, newG)
  where
    (newP, newG) = mutatePopulation (take amount $ cycle p) g
    amount = populationSize - (length $ parents ++ children)
    p = parents ++ children

-- populations diff
-- get parents
-- crossover
-- combinePopulation
gaLoopBody :: RandomGen g => (Population, g, Image PixelRGBA8) -> (Population, g, Image PixelRGBA8)
gaLoopBody (p, g, img) = (newP, newG, img)
  where
    parents = getParents parentsAmount $ populationDiff p img
    children = offspring parents
    (newP, newG) = combinePopulation parents children g
    
runGANTimes :: RandomGen g => Int -> (Population, g, Image PixelRGBA8) -> (Population, g, Image PixelRGBA8)
runGANTimes n t = last $ take n $ iterate gaLoopBody t

finalChromosome :: Population -> Image PixelRGBA8 -> Chromosome
finalChromosome p img = head $ getParents 1 $ populationDiff p img

finalImage :: RandomGen g => Image PixelRGBA8 -> g -> Image PixelRGBA8
finalImage img g0 = renderChromosome finalC
  where
    (initP, g1) = initPopulation g0
    (finalP, _, _) = runGANTimes gaN (initP, g1, img)
    finalC = finalChromosome finalP img

percentFitness :: Integer -> Float
percentFitness diff = ratio
  where
    (width, height) = imageSize
    [w, h] = toInteger <$> [width, height]
    maxDiff = 255*3 * w * h
    ratio = fromIntegral(maxDiff) / fromIntegral(diff)
    -- oneP = 100 / fromIntegral(maxDiff)
