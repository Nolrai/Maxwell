module Main (main, myImage) where
import Diagrams.Backend.SVG.CmdLine (mainWith)
import Graphics.Image as Image
-- import Graphics.Image.Interface as Image ()
import ImageFunction (imageFunctions)
import Prelude
import GHC.Real (fromIntegral)

main :: IO ()
main = do
  doImage `mapM_` imageFunctions
  doImageShifted `mapM_` imageFunctions

doImage, doImageShifted :: (String, Double -> Double -> Pixel RGB Double) -> IO ()
doImageShifted (name, function) = writeImage (name <> "_shifited.png") (myImage (shiftAlernateRows function))
doImage (name, function) = writeImage (name <> ".png") (myImage (unshiftedRows function))

size :: Num a => a
size = 255

myImage :: ((Int, Int) -> Pixel Image.RGB Double) -> Image RPU Image.RGB Double
myImage = makeImageR RPU (size, size)

unshiftedRows :: (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
unshiftedRows imageFunction (x, y) = 
    if 0 <= x' && x' <= 1
      then imageFunction y' x'
      else imageFunction 0 0
  where
  x' = (((fromIntegral x / size) - (1/2)) / sin (y' * pi)) + 1/2
  y' = fromIntegral y / size

shiftAlernateRows :: (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
shiftAlernateRows imageFunction (x, y) = imageFunction (fromIntegral x / size) (fromIntegral y' / size)
  where
    y'
      | even x = y
      | y < (size `div` 2) = y + (size `div` 2)
      | otherwise = y - (size `div` 2)