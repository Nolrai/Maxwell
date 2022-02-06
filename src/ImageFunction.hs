{-# LANGUAGE NoImplicitPrelude #-}

module ImageFunction (imageFunctions) where
import Relude
import Data.Colour.RGBSpace.HSV
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant
import Graphics.Image as Image ( RGB, Pixel(PixelRGB) )
import Data.Colour.SRGB.Linear as Colour ( RGB(RGB), toRGB )
import Linear ( V3(V3), (*!) )
import InputManipulation (rotateFloat, yellowToRed)

imageFunctions :: [([Char], Double -> Double -> Pixel Image.RGB Double)]
imageFunctions =
  [ ("cieLABR",  imageFunctionCieLabR)
  , ("cieLAB",   imageFunctionCieLab)
  , ("HSV0",      imageFunctionHsv0)
  , ("HSV1",     imageFunctionHsv1)
  ]

fromSphere :: Floating a => a -> a -> V3 a
fromSphere phi theta = V3 (sin (phi * pi / 2)) (r * sin (theta * 2 * pi)) (r * cos (theta * 2 * pi))
  where
    r = sin (phi * pi)

toDiagonal :: Num a => a -> a -> a -> V3 (V3 a)
toDiagonal a b c = V3 (V3 a 0 0) (V3 0 b 0) (V3 0 0 c)

cieLAB' :: Double -> Double -> Double -> Colour Double
cieLAB' = cieLAB d65

imageFunctionCieLabR :: Double -> Double -> Pixel Image.RGB Double
imageFunctionCieLabR phi theta = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = toRGB $ cieLAB' l a b
    V3 a l b = (xyz *! toDiagonal 35 80 40) + V3 60 (-40) 0
    xyz = fromSphere phi theta

imageFunctionCieLab :: Double -> Double -> Pixel Image.RGB Double
imageFunctionCieLab phi theta= let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = toRGB $ cieLAB' l a b
    V3 l a b = (xyz *! toDiagonal 70 40 40) + V3 30 0 0
    xyz = fromSphere phi theta

imageFunctionHsv0 :: Double -> Double -> Pixel Image.RGB Double
imageFunctionHsv0 phi theta = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = hsv (theta * 360) (0.9 * sin (phi * pi)) phi

imageFunctionHsv1 :: Double -> Double -> Pixel Image.RGB Double
imageFunctionHsv1 phi theta = imageFunctionHsv0 phi (rotateFloat yellowToRed theta)
