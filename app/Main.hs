{-# LANGUAGE NoImplicitPrelude #-}
module Main (main, myImage) where
import Graphics.Image as Image
    ( makeImageR, writeImage, RGB, Image, Pixel, RPU(..) )
import ImageFunction (imageFunctions)
import Relude
import System.Directory
import Text.ParserCombinators.ReadPrec (step)

main :: IO ()
main = do
  createDirectoryIfMissing True prefix
  args <- getArgs
  let resolution = fromMaybe 30 $ readMaybe =<< viaNonEmpty head args 
  sequence_ $ do
    outer <- [doImageCurved, doImageFlat]
    outer resolution <$> imageFunctions

doImage :: 
  String -> 
  (Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double) -> 
  Int ->
  (String, Double -> Double -> Pixel Image.RGB Double) ->
  IO ()
doImage suffix rows resolution (name, function ) = writeImage (name <> suffix) (myImage resolution rows function)

doImageCurved, doImageShifted, doImageFlat :: Int -> (String, Double -> Double -> Pixel RGB Double) -> IO ()
doImageShifted    = doImage "_shifited.png"   shiftAlernateRows
doImageCurved     = doImage "_curved.png"     curvedRows
doImageFlat       = doImage "_flat.png"       flatRows
doImageSingleton  = doImage "_singleton.png"  singletonRows

myImage :: 
  Int -> 
  (Int -> (Double -> Double -> Pixel RGB Double) -> (Int, Int) -> Pixel RGB Double) ->
  (Double -> Double -> Pixel Image.RGB Double) -> 
  Image RPU Image.RGB Double
myImage size rows f = makeImageR RPU (size, size) (rows size f)

shifted :: (Integral a) => a -> a -> a
shifted size x = if x > size' then x - size' else x + size'
  where
    size' = size `div` 2

singletonRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
singletonRows size imageFunction (x, _) = imageFunction (1/2) x'
  where
  x' :: Double
  x' = fromIntegral (shifted size x) / fromIntegral size

flatRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
flatRows size imageFunction (x, y) = imageFunction y' x'
  where
  x' = fromIntegral x / fromIntegral size
  y' = fromIntegral y / fromIntegral size

curvedRowsShifted' :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
curvedRowsShifted' size imageFunction (x, y) = curvedRows size imageFunction (shifted size x, y)

curvedRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
curvedRows size imageFunction (x, y) = 
    if 0 <= x' && x' <= 1
      then imageFunction y' x'
      else imageFunction 0 0
  where
  x' = (((fromIntegral x / fromIntegral size) - (1/2)) / sin (y' * pi)) + 1/2
  y' = fromIntegral y / fromIntegral size

shiftAlernateRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
shiftAlernateRows size imageFunction (x, y) = flatRows size imageFunction (x, y')
  where
    y' :: Int
    y' = if even x then y else shifted (size `div` 2) y