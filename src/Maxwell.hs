{- |
Copyright: (c) 2021 Chris Upshaw
SPDX-License-Identifier: MIT
Maintainer: Chris Upshaw <chrisaupshaw@gmail.com>

vectorfields in color
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Maxwell (myPicture, toXYZfromOK, toLab, XYZ(..), Lab (..), LMS(..)) where

import Diagrams.Prelude hiding (light)
import Diagrams.Backend.SVG.CmdLine ( B )
import Data.Colour.RGBSpace.HSL ( hsl )
import Data.Colour.RGBSpace ( uncurryRGB )
import Data.Colour.CIE ( cieLAB, cieXYZ ) 
import Data.Colour.CIE.Illuminant ( d65 )

type Color' = Colour Double

myCircle :: Colour Double -> Colour Double -> Diagram B
myCircle a b = bot # lw none --vcat [top, bot] 
  where
    bot = hcat $ myRect <$> [a,b]
    myRect c = rect (1/2) 1.5 # fillColor c

-- myGradientL :: Color' -> Color' -> Texture Double
-- myGradientL a b = defaultLG & _LG . lGradStops .~ stops a b

-- stops :: Color' -> Color' -> [GradientStop Double]
-- stops a b = [flip GradientStop 0 $ SomeColor a, flip GradientStop 1 $ SomeColor b]

row :: [Color'] -> Diagram B
row colorList = hcat (zipWith myCircle colorList (drop 1 $ cycle colorList))

cieLABColorList :: [Color']
cieLABColorList = shiftR 2 . reverse $ (\x -> cieLAB d65 90 (100 * sin (2 * pi * x)) (100 * cos (2 * pi * x))) <$> fromZeroToOne

okLABColorList :: [Color']
okLABColorList = shiftR 2 . reverse $ (\x -> okLAB 90 (100 * sin (2 * pi * x)) (100 * cos (2 * pi * x))) <$> fromZeroToOne

shiftR :: Int -> [a] -> [a]
shiftR n l = shiftL (length l - n) l

shiftL :: Int -> [a] -> [a]
shiftL n l = drop n l <> take n l

hslColorList :: [Color']
hslColorList = (\x -> uncurryRGB sRGB $ hsl (x * 360) 0.9 0.5) <$> fromZeroToOne

sRGBColorList :: [Color']
sRGBColorList = (\x -> if x < 1/3 then blend (x * 3) limegreen red else if x < 2/3 then blend (x * 3 - 1) blue limegreen else blend (x * 3 - 2) red blue) <$> fromZeroToOne

fromZeroToOne :: [Double]
fromZeroToOne = drop 1 (enumFromThenTo 0 (1/18) 1)

myPicture :: Diagram B
myPicture = vcat $ row <$> reverse [okLABColorList, cieLABColorList, hslColorList, sRGBColorList]

okLAB :: Double -> Double -> Double -> Colour Double
okLAB l a b = cieXYZ x y z
  where
    v = toXYZfromOK (toLab l a b)
    x = v X
    y = v Y
    z = v Z

toLab :: a -> a -> a -> Lab -> a
toLab light aStar bStar lab = 
  case lab of
    Light -> light
    AStar -> aStar
    BStar -> bStar

toXYZfromOK :: (Lab -> Double) -> XYZ -> Double
toXYZfromOK lab = toXYZfromLMS $ (^ (3 :: Int)) <$> toLMSFromOk (lab Light) (lab AStar) (lab BStar)

toLMSFromOk :: Double -> Double -> Double -> LMS -> Double
toLMSFromOk light aStar bStar lms = light * mrow Light + aStar * mrow AStar + bStar * mrow BStar
  where
    mrow = m1 lms

toXYZfromLMS :: (LMS -> Double) -> (XYZ -> Double)
toXYZfromLMS f xyz = f LL * mrow LL + f MM * mrow MM + f SS * mrow SS
  where
    mrow = m2 xyz

data XYZ = X | Y | Z
data LMS = LL | MM | SS
data Lab = Light | AStar | BStar

m1 :: LMS -> Lab -> Double
m1 LL Light = 0.8189330101
m1 MM Light = 0.0329845436
m1 SS Light = 0.0482003018

m1 LL AStar = 0.3618667424
m1 MM AStar = 0.9293118715
m1 SS AStar = 0.0482003018

m1 LL BStar = -0.1288597137
m1 MM BStar = 0.0361456387
m1 SS BStar = 0.6338517070

m2 :: XYZ -> LMS -> Double
m2 X LL = 0.2104542553
m2 Y LL = 1.9779984951
m2 Z LL = 0.0259040371
 
m2 X MM = 0.7936177850
m2 Y MM = -2.4285922050
m2 Z MM = 0.7827717662
 
m2 X SS = -0.0040720468
m2 Y SS = 0.4505937099
m2 Z SS = -0.8086757660