{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module InputManipulation (onF, onRotate, rotateFloat, from01ToNeg1Pos1, fromNeg1Pos1To01, smoosh, affine, yellowToRed) where

import Relude

onF :: (a -> b1) -> (b2 -> a) -> (b1 -> b2) -> a -> a
onF f unf g = unf . g . f

smoosh :: Floating a => a -> a -> a -> a
smoosh a b x = signum x * abs x ** logBase a b

onRotate :: RealFrac b2 => b2 -> (b2 -> b2) -> b2 -> b2
onRotate amount = onF (rotateFloat amount) (rotateFloat (-amount))

from01ToNeg1Pos1 :: Double -> Double
from01ToNeg1Pos1 = affine 2.0 (-1)
fromNeg1Pos1To01 :: Double -> Double
fromNeg1Pos1To01 = unAffine 2.0 (-1)

affine :: Num a => a -> a -> a -> a
affine a b x = x * a + b

unAffine :: Floating a => a -> a -> a -> a
unAffine a b x = (x - b) / a

frem :: RealFrac a => a -> a -- frem for floating remainder, like fmod from C
frem a = if x < 0 then 1 + x else x
  where
    (_ :: Int, x) = properFraction a

-- wraps [0 1] into a circle then rotates input by that amount.
rotateFloat :: RealFrac a => a -> a -> a
rotateFloat amount input = frem (input + amount)

yellowToRed :: Double
yellowToRed = -5/30 -- A.K.A. 60 degrees (of 360)