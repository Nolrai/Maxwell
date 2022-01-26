module Main (main, myImage) where
import Maxwell (myPicture, okLAB, okLAB', cieLAB')
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
myImage = makeImageR RPU (18, 18) (\ (x, y) -> imageFunction (fromIntegral x / 18) (fromIntegral y / 18))

imageFunction :: Double -> Double -> Pixel Image.RGB Double
imageFunction x y = let p = PixelRGB red green blue in Prelude.trace (show p) p
  where
    -- Colour.RGB red green blue = toRGB $ okLAB 0.3 (sin (x * 2 * pi)) (cos (y * 2 * pi))
    Colour.RGB red green blue = toRGB $ cieLAB' 80 (50 * cos (x * 2 * pi)) (50 * cos (y * 2 * pi))
    -- l = l'/2 + 0.5
    -- (V3 l' a b) = inverseProjection 1 (V2 (x * pi/2) (y * pi))