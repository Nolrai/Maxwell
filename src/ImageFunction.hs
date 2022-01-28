{-# LANGUAGE NoImplicitPrelude #-}

module ImageFunction (imageFunctions) where
import Relude
import Maxwell ( hsv, hsl, hsvView, cieLAB', okLAB )
import Graphics.Image as Image ( RGB, Pixel(PixelRGB) )
import Data.Colour.SRGB.Linear as Colour ( RGB(RGB), toRGB )
import Turbo ( turbo )

imageFunctions :: [([Char], Double -> Double -> Pixel Image.RGB Double)]
imageFunctions =
  [ ("okLab",    imageFunctionOkayLab)
  , ("cieLABR",  imageFunctionCieLabR)
  , ("cieLAB",   imageFunctionCieLab)
  , ("HSL",      imageFunctionHsl)
  , ("HSV",      imageFunctionHsv)
  , ("turbo1D",  imageFunctionTurbo)
  , ("turboHSV", imageFunctionTurboHsv)
  ]

imageFunctionOkayLab :: Double -> Double -> Pixel Image.RGB Double
imageFunctionOkayLab  x y = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = toRGB $ okLAB (0.3 + 0.7 * sin (x * pi / 2)) (0.4 * sin (x * pi) * sin (y * 2 * pi)) (0.4 * sin (x * pi) * cos (y * 2 * pi))

imageFunctionCieLabR :: Double -> Double -> Pixel Image.RGB Double
imageFunctionCieLabR x y = let p = PixelRGB red green blue in p
  where
    Colour.RGB red green blue = toRGB $ cieLAB' (0.6 + 0.35 * sin (x * pi) * sin (y * 2 * pi)) (-0.4 + 0.8 * sin (x * pi / 2)) (40 * sin (x * pi) * cos (y * 2 * pi))

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
    (h, _s, _v) = hsvView . toRGB $ turbo y