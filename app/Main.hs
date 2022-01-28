{-# LANGUAGE NoImplicitPrelude #-}
module Main (main, myImage) where
import Graphics.Image as Image hiding (rows)
import ImageFunction (imageFunctions)
import Relude

main :: IO ()
main = do
  args <- getArgs
  let resolution = fromMaybe 30 $ readMaybe =<< viaNonEmpty head args 
  sequence_ $ do
    outer <- [doImageCurved, doImageShifted, doImageFlat, doImageSingleton]
    outer resolution  <$> imageFunctions

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
  (Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double) ->
  (Double -> Double -> Pixel Image.RGB Double) -> 
  Image RPU Image.RGB Double
myImage size rows f = makeImageR RPU (size, size) (rows size f)

singletonRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
singletonRows size imageFunction (x, _) = imageFunction (1/2) x''
  where
  x', x'' :: Double
  x' = fromIntegral x / fromIntegral size
  x'' = 
    if x' < 1/2
      then x' + 1/2
      else x' - 1/2

flatRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
flatRows size imageFunction (x, y) = imageFunction y' x'
  where
  x' = fromIntegral x / fromIntegral size
  y' = fromIntegral y / fromIntegral size

curvedRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
curvedRows size imageFunction (x, y) = 
    if 0 <= x'' && x'' <= 1
      then imageFunction y' x''
      else imageFunction 0 0
  where
  x' = (((fromIntegral x / fromIntegral size) - (1/2)) / sin (y' * pi)) + 1/2
  y' = fromIntegral y / fromIntegral size
  x'' = 
  if x' < 1/2
    then x' + 1/2
    else x' - 1/2


shiftAlernateRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
shiftAlernateRows size imageFunction (x, y) = imageFunction (fromIntegral x / fromIntegral size) (fromIntegral y' / fromIntegral size)
  where
    y' :: Int
    y'
      | even x = y
      | y < (size `div` 2) = y + (size `div` 2)
      | otherwise = y - (size `div` 2)