module Main (main, myImage) where
import Maxwell (myPicture, okLAB)
import Globe (inverseProjection)
import Diagrams.Backend.SVG.CmdLine (mainWith)
import Graphics.Image as Image
import Graphics.Image.Interface as Image
import Data.Colour.SRGB.Linear as Colour
import Linear.V3
import Linear.V2

main :: IO ()
main = 
    writeImage "sample.png" myImage

myImage :: Image RPU Image.RGB Double
myImage = makeImageR RPU (1080, 1920) (\ (x, y) -> imageFunction (fromIntegral x / 1080) (fromIntegral y / 1920))

imageFunction :: Double -> Double -> Pixel Image.RGB Double
imageFunction x y = PixelRGB red green blue 
  where
    Colour.RGB red green blue = toRGB $ okLAB l a b
    l = l'/2 + 0.5
    (V3 l' a b) = inverseProjection 1 (V2 (x * pi/2) (y * pi))