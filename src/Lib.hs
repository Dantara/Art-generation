{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Lib
    ( outputFunc
    ) where

import Control.Monad.State.Lazy
import Codec.Picture
import System.Random
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture.RGBA8
import Data.Sort
import Control.Parallel.Strategies
import GHC.Generics (Generic)
import Control.DeepSeq
import Graphics.Rasterific.Immediate
import Control.Monad.Primitive
import Control.Monad.ST
import Codec.Picture.Extra

-- Size of initial image
imageSize :: (Int, Int)
imageSize = (512, 512)

-- Size of chunk
chunkSize :: (Int, Int)
chunkSize = (8, 8)

-- Amount of vertices in polygon
polygonVertices :: Int
polygonVertices = 3

-- Size of chromosome, in other words
-- number of polygons in one chunk
chromosomeSize :: Int
chromosomeSize = 6

-- Size of population for one chunk
populationSize :: Int
populationSize = 12

-- Amount of parents
parentsAmount :: Int
parentsAmount = 2

-- Amount of genetic algorithm iterations for one chunk
gaN :: Int
gaN = 200

-- Background color of image
baseBackgroundColor :: PixelRGBA8
baseBackgroundColor = PixelRGBA8 0 0 0 255

-- Path to input image
inputImagePath :: String
inputImagePath = "input.png"

-- Path to output image
outputImagePath :: String
outputImagePath = "output1.png"

data Polygon = Polygon [Point] PixelRGBA8 deriving (Show, Generic, NFData)
type Gene = Polygon
type Chromosome = [Gene]
type Population = [Chromosome]

instance NFData (V2 a) where
  rnf x = seq x ()

instance NFData PixelRGBA8 where
  rnf x = seq x ()

outputFunc :: IO ()
outputFunc = do
  g <- newStdGen
  imageLoad <- readImage inputImagePath

  case imageLoad of
    Left err -> putStrLn err
    Right image -> do
      let inputImg = fromDynamicImage image
          outputImg = divideAndConquer inputImg g

      putStr "Final fitness is: "
      putStrLn $ show (percentFitness $ imageDiff inputImg outputImg) ++ "%"
      writePng outputImagePath outputImg

addNRandom :: RandomGen g => Int -> (g -> (a, g)) -> ([a], g) -> ([a], g)
addNRandom 0 _ x = x
addNRandom n f (ys, g) = addNRandom (n-1) f (y : ys, newG)
  where
    (y, newG) = f g

randomPolygon :: RandomGen g => g -> (Polygon, g)
randomPolygon g0 = (Polygon coords color, g2)
  where
    (coords, g1) = nRandomCoords polygonVertices g0
    (color, g2) = randomColor g1

initChromosome :: RandomGen g => g -> (Chromosome, g)
initChromosome g = addNRandom chromosomeSize randomPolygon ([], g)

initPopulation :: RandomGen g => g -> (Population, g)
initPopulation g = addNRandom populationSize initChromosome ([], g)

renderChromosome :: Chromosome -> Image PixelRGBA8
renderChromosome c = runST
  $ runDrawContext width height baseBackgroundColor
  $ mapM_ polygonToDrawContext c
  where
    (width, height) = chunkSize

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
      (x_max, y_max) = chunkSize

polygonToDrawContext :: PrimMonad m => Polygon -> DrawContext m PixelRGBA8 ()
polygonToDrawContext p = fillWithTexture FillWinding texture geometry
  where
    texture = uniformTexture color
    geometry = polygon coords
    (Polygon coords color) = p

randomColor :: RandomGen g => g -> (PixelRGBA8, g)
randomColor = runState (liftM4 PixelRGBA8 r r r r)
  where
    r = state $ randomR (0, 255)

randomCoord :: RandomGen g => g -> (Point, g)
randomCoord = runState (liftM2 V2 r1 r2)
  where
    r1 = state $ randomR (0, realToFrac $ fst chunkSize)
    r2 = state $ randomR (0, realToFrac $ snd chunkSize)

nRandomCoords :: RandomGen g => Int -> g -> ([Point], g)
nRandomCoords n g = addNRandom n randomCoord ([], g)

mutateColorById :: RandomGen g => Int -> PixelRGBA8 -> g -> (PixelRGBA8, g)
mutateColorById n color g0
 | n == 1 = (PixelRGBA8 rand g b a, newG)
 | n == 2 = (PixelRGBA8 r rand b a, newG)
 | n == 3 = (PixelRGBA8 r g rand a, newG)
 | n == 4 = (PixelRGBA8 r g b rand, newG)
 | otherwise = (color, g0)
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
    1 -> (Polygon newCoords color, g3)
    2 -> (Polygon coords newColor, g3)
    3 -> randomPolygon g0
    4 -> (Polygon randCoords color, g3)
    5 -> (Polygon coords randColor, g3)
    _ -> (gene, g0)
  where
      index :: Integer
      (Polygon coords color) = gene
      (index, g1) = randomR (1, 5) g0
      (g2, g3) = split g1
      (newCoords, _) = mutateCoords coords g2
      (newColor, _) = mutateColor color g2
      (randCoords, _) = nRandomCoords polygonVertices g2
      (randColor, _) = randomColor g2

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
populationDiff cs img = parMap rdeepseq (\c -> chromosomeDiff c img) cs

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

mutateChromosomes :: RandomGen g => [Chromosome] -> g -> [Chromosome]
mutateChromosomes [] _ = []
mutateChromosomes (c:cs) g = newC : (mutateChromosomes cs g2)
  where
    (newC, _) = mutateChromosome c g1
    (g1, g2) = split g

mutatePopulation :: RandomGen g => Population -> g -> (Population, g)
mutatePopulation cs g = (newCs, g2)
  where
    newCs = mutateChromosomes cs g1
    (g1, g2) = split g

combinePopulation :: RandomGen g => [Chromosome] -> [Chromosome] -> g -> (Population, g)
combinePopulation parents children g = (newP <> p, newG)
  where
    (newP, newG) = mutatePopulation (take amount $ cycle p) g
    amount = populationSize - (length p)
    p = parents ++ children

gaLoopBody :: RandomGen g => (Population, g, Image PixelRGBA8) -> (Population, g, Image PixelRGBA8)
gaLoopBody (p, g, img) = (newP, g2, img)
  where
    parents = getParents parentsAmount $! populationDiff p img
    children = offspring parents
    (newP, _) = combinePopulation parents children g1
    (g1, g2) = split g

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
percentFitness diff = 100 * (1 - ratio)
  where
    (width, height) = chunkSize
    [w, h] = toInteger <$> [width, height]
    maxDiff = 255*3*w*h
    ratio = fromIntegral(diff) / fromIntegral(maxDiff)

type Chunks = [[Image PixelRGBA8]]

divideByY :: Int -> Image PixelRGBA8 -> [Image PixelRGBA8]
divideByY y img
  | y >= imgHeight = []
  | otherwise = (crop 0 y chunkHeight chunkHeight img) : (divideByY (y + chunkHeight) img)
  where
    (_, chunkHeight) = chunkSize
    (_, imgHeight) = imageSize

divideByX :: Int -> Image PixelRGBA8 -> [Image PixelRGBA8]
divideByX x img
  | x >= imgWidth = []
  | otherwise = byX : (divideByX (x + chunkWidth) img)
  where
    byX = crop x 0 chunkWidth imgHeight img
    (chunkWidth, _) = chunkSize
    (imgWidth, imgHeight) = imageSize

divideImage :: Image PixelRGBA8 -> Chunks
divideImage img = map (divideByY 0) $ divideByX 0 img

gaList :: RandomGen g => [Image PixelRGBA8] -> g -> [Image PixelRGBA8]
gaList [] _ = []
gaList (x:xs) g0 = (finalImage x g1) : (gaList xs g2)
  where
    (g1, g2) = split g0

gaChunks :: RandomGen g => Chunks -> g -> Chunks
gaChunks [] _ = []
gaChunks (x:xs) g0 = (gaList x g1) : (gaChunks xs g2)
  where
    (g1, g2) = split g0

combineChunks :: Chunks -> Image PixelRGBA8
combineChunks = beside . map below

divideAndConquer :: RandomGen g => Image PixelRGBA8 -> g -> Image PixelRGBA8
divideAndConquer img g = combineChunks parCalculated
  where
    divided = divideImage img
    calculated = gaChunks divided g
    parCalculated = calculated `using` parList rdeepseq
