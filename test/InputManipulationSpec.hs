{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, MultiWayIf #-}

module InputManipulationSpec (spec) where

import Relude
import InputManipulation
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Colour.Names as Colours
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Exception (ioError)

newtype ZeroToOne = ZeroToOne Double
  deriving (Show)
newtype Neg1Pos1 = Neg1Pos1 Double
  deriving (Show)

instance Arbitrary ZeroToOne where
  arbitrary = ZeroToOne <$> choose (0, 1)

instance Arbitrary Neg1Pos1 where
  arbitrary = Neg1Pos1 <$> choose (-1, 1)

infix 1 ~=?
(~=?) :: Double -> Double -> Expectation
actual ~=? expected = 
  if abs (actual - expected) < exp (-10 :: Double)
  then expected `shouldBe` expected 
  else actual `shouldBe` expected 

spec :: Spec
spec = do
  describe "yellowToRed" $
    it "should be negative of yellow hue" $ do
      let yellow_hue'= hue $ toSRGB Colours.yellow
      let yellow_hue = yellow_hue'/360
      yellowToRed ~=? -yellow_hue
  describe "yellowToRed" $
    it "should take yellow to red" $ do
      let red_hue' = hue $ toSRGB Colours.red
      let red_hue = red_hue'/360 
      let yellow_hue'= hue $ toSRGB Colours.yellow
      let yellow_hue = yellow_hue'/360
      rotateFloat yellowToRed yellow_hue ~=? (red_hue :: Double)
  describe "rotateFloat" $ do
    it "should produce non-neg values" $ do
      property $ \ (Neg1Pos1 amount) (ZeroToOne x) ->
       amount /= 1 ==> rotateFloat amount x `shouldSatisfy` (>= (0 :: Double))
    it "should produce values less than or equal to 1" $ do
      property $ \ (Neg1Pos1 amount) (ZeroToOne x) -> 
       amount /= 1 ==> rotateFloat amount x `shouldSatisfy` (<= (1 :: Double))
  describe "smoosh a b" $ do
    it "greater than -1" . 
      property $ \ (ZeroToOne a) (ZeroToOne b) (Neg1Pos1 x) -> 
        a /= 0 && b /= 0 ==> smoosh a b x `shouldSatisfy` (> (-1 :: Double))
    it "less than 1" . 
      property $ \ (ZeroToOne a) (ZeroToOne b) (Neg1Pos1 x) -> 
        a /= 0 && b /= 0 ==> smoosh a b x `shouldSatisfy` (> (-1 :: Double))
    it "map 1 to 1" . 
      property $ \ (ZeroToOne a) (ZeroToOne b) -> 
        a /= 0 && b /= 0 ==> smoosh a b 1 ~=? (1 :: Double)
    it "map 0 to 0" . 
      property $ \ (ZeroToOne a) (ZeroToOne b) -> 
        a /= 0 && b /= 0 ==> smoosh a b 0 ~=? (0 :: Double)
    it "map a to b" . 
      property $ \ (ZeroToOne a) (ZeroToOne b) -> 
        a /= 0 && b /= 0 ==> smoosh a b a ~=? (b :: Double)
    it "be inverse of smosh b a" . 
      property $ \ (ZeroToOne a) (ZeroToOne b) (Neg1Pos1 x) -> 
        a /= 0 && b /= 0 ==> smoosh b a (smoosh a b x) ~=? x
  describe "fromNeg1Pos1To01" $ do
    it "takes -1 to 0" $
      fromNeg1Pos1To01 (-1) ~=? 0
    it "takes 1 to 1" $ 
      (fromNeg1Pos1To01 1 :: Double) ~=? 1
    it "is affine " . 
      property $ \ (Neg1Pos1 a) (Neg1Pos1 b) (Neg1Pos1 c) (Neg1Pos1 d) -> do
        c - d /= 0 ==> (fromNeg1Pos1To01 a - fromNeg1Pos1To01 b) / (fromNeg1Pos1To01 c - fromNeg1Pos1To01 d) ~=? (a - b)/(c - d)
    it "is inverse of from01ToNeg1Pos1" .
      property $ \ (ZeroToOne a) -> do
        fromNeg1Pos1To01 (from01ToNeg1Pos1 a) ~=? a
  describe "from01ToNeg1Pos1" $ do
    it "takes 0 to -1" $
      from01ToNeg1Pos1 0 ~=? (-1)
    it "takes 1 to 1" $
      from01ToNeg1Pos1 1 ~=? 1
    it "is affine " . 
      property $ \ (ZeroToOne a) (ZeroToOne b) (ZeroToOne c) (ZeroToOne d) ->
        c - d /= 0 ==> (from01ToNeg1Pos1 a - from01ToNeg1Pos1 b) / (from01ToNeg1Pos1 c - from01ToNeg1Pos1 d) ~=? (a - b)/(c - d)
    it "is inverse of fromNeg1Pos1To01" .
      property $ \ (Neg1Pos1 a) ->
        from01ToNeg1Pos1 (fromNeg1Pos1To01 a) ~=? a