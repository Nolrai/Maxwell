module ImageFunction (imageFunctions) where
import Maxwell (myPicture, okLAB, okLAB', cieLAB', hsl, hsv, hslView, hsvView)
import Globe (inverseProjection)
import Diagrams.Backend.SVG.CmdLine (mainWith)
import Graphics.Image as Image
import Graphics.Image.Interface as Image
import Data.Colour.SRGB.Linear as Colour
import Linear.V3
import Linear.Vector ((^*))
import Linear.Metric (dot)
import Turbo
import Data.Vector as V ( Vector )

sumV3 :: Num a => V3 a -> a
sumV3 (V3 x y z) = x + y + z

imageFunctions =
  [ ("okLab", imageFunctionOkayLab)
  , ("cieLAB", imageFunctionCieLab)
  , ("HSL", imageFunctionHsl)
  , ("HSV", imageFunctionHsv)
  , ("turbo1D", imageFunctionTurbo)
  , ("turboHSV", imageFunctionTurboHsv)
  ]

imageFunctionOkayLab :: Double -> Double -> Pixel Image.RGB Double
imageFunctionOkayLab  x y = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = toRGB $ okLAB (0.3 + 0.7 * sin (x * pi / 2)) (0.4 * sin (x * pi) * sin (y * 2 * pi)) (0.4 * sin (x * pi) * cos (y * 2 * pi))

imageFunctionCieLab :: Double -> Double -> Pixel Image.RGB Double
imageFunctionCieLab x y = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = toRGB $ cieLAB' (30 + 70 * sin (x * pi / 2)) (40 * sin (x * pi) * sin (y * 2 * pi)) (40 * sin (x * pi) * cos (y * 2 * pi))

imageFunctionHsl :: Double -> Double -> Pixel Image.RGB Double
imageFunctionHsl x y = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = hsl (y * 360) (0.9 * sin (x * pi)) x

imageFunctionHsv :: Double -> Double -> Pixel Image.RGB Double
imageFunctionHsv x y = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = hsv (y * 360) (0.9 * sin (x * pi)) x

imageFunctionTurbo :: Double -> Double -> Pixel Image.RGB Double
imageFunctionTurbo x _ = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = toRGB $ turbo x

imageFunctionTurboHsv :: Double -> Double -> Pixel Image.RGB Double
imageFunctionTurboHsv x y = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = hsv h (0.9 * sin (x * pi)) x
    (h, s, v) = hsvView . toRGB $ turbo y