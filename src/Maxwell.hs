{- |
Copyright: (c) 2021 Chris Upshaw
SPDX-License-Identifier: MIT
Maintainer: Chris Upshaw <chrisaupshaw@gmail.com>

vectorfields in color
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Maxwell (myPicture, toXYZfromOK, okLAB) where

import Diagrams.Prelude hiding (light)
import Diagrams.Backend.SVG.CmdLine ( B )
import Data.Colour.RGBSpace.HSL ( hsl )
import Data.Colour.RGBSpace ( uncurryRGB )
import Data.Colour.CIE ( cieLAB, cieXYZ ) 
import Data.Colour.CIE.Illuminant ( d65 )
import Linear.Matrix ( M33, inv33, (!*) )

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
cieLABColorList = shiftR 2 . reverse $ (\x -> cieLAB d65 50 (100 * sin (2 * pi * x)) (100 * cos (2 * pi * x))) <$> fromZeroToOne

redColorList :: [Color']
redColorList = shiftR 2 . reverse $ (\x -> cieLAB d65 (50 + 50 * sin (2 * pi * x)) 100 (100 * cos (2 * pi * x))) <$> fromZeroToOne

okLABColorList :: [Color']
okLABColorList = shiftR 2 . reverse $ (\x -> okLAB 0.5 (sin (2 * pi * x)) (cos (2 * pi * x))) <$> fromZeroToOne

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
myPicture = vcat $ row <$> [cieLABColorList, redColorList, okLABColorList]

okLAB :: Double -> Double -> Double -> Colour Double
okLAB l a b = cieXYZ x y z
  where
    V3 x y z = toXYZfromOK (V3 l a b)

toXYZfromOK :: V3 Double -> V3 Double
toXYZfromOK lab = m1 !* ((^ (3 :: Int)) <$> m2 !* lab)

m1, m2 :: M33 Double
m1 = inv33 $
  V3 
    (V3 0.8189330101 0.3618667424 (-0.1288597137))
    (V3 0.0329845436 0.9293118715   0.0361456387)
    (V3 0.0482003018 0.2643662691   0.6338517070)

m2 = inv33 $
  V3
    (V3 0.2104542553   0.7936177850  (-0.0040720468))
    (V3 1.9779984951 (-2.4285922050)   0.4505937099)
    (V3 0.0259040371   0.7827717662  (-0.8086757660))

