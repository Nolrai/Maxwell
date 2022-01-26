{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module MaxwellSpec (spec) where
import Test.Hspec 
import Text.Printf ( printf )
import Test.QuickCheck
import Maxwell
import Data.Colour 
import Data.Colour.CIE 
import Data.Colour.SRGB.Linear ( toRGB, RGB(RGB), sRGBGamut )
import Data.Colour.RGBSpace
import Linear.V3 (V3(V3))
import Control.Lens

showDigits :: Int -> Double -> String 
showDigits n = printf ("%." <> show n <> "f")

showXYZ :: V3 Double -> String
showXYZ v = showDigits 2 (v ^. _1) <> ", " <> showDigits 1 (v ^. _2) <> ", " <> showDigits 1 (v ^. _3)

showLAB :: V3 Double -> String
showLAB lab = printf "L %f a* %f b* %f" (lab ^. _1) (lab ^. _2) (lab ^. _3)

showRBG (RGB r b g) = showXYZ (V3 r g b)

okLABTest :: V3 Double -> String -> Spec
okLABTest lab result = 
  it (showLAB lab)
    $ showXYZ (toXYZfromOK lab) `shouldBe` result

spec :: Spec
spec = do
  describe "toXYZfromOk" $
    describe "should match the examples from https://bottosson.github.io/posts/oklab/" $ do
      okLABTest (V3 1     0          0)      "0.950, 1.000, 1.088"
      okLABTest (V3 0.450 1.236    (-0.019)) "1.001, -0.000, -0.000"
      okLABTest (V3 0.922 (-0.671)   0.263)  "0.000, 1.001, 0.001"
      okLABTest (V3 0.153 (-1.415) (-0.449)) "0.001, 0.000, 1.002"
  describe "okLAB and okLAB'" $
    it "should make the same colors" . property $
    \ (LAB l a b) ->  showRBG (toRGB (okLAB l a b)) `shouldBe` showRBG (toRGB (okLAB' l a b))
  focus $ describe "cieLAB'" $
    it "should make colors inGamut" . property $
    \ (x, y) -> cieLAB' (50 :: Double) x y `shouldSatisfy` inGamut sRGBGamut
  

data LAB a = LAB a a a 
  deriving stock (Show, Eq)

toLAB :: V3 Int -> LAB Double
toLAB v3 = LAB l a b
  where V3 l a b = snd . properFraction . (/100) . fromIntegral <$> v3

instance Arbitrary (LAB Double) where
  arbitrary = toLAB <$> (V3 <$> arbitrary <*> arbitrary <*> arbitrary)
  shrink (LAB l a b) = toLAB <$> (V3 <$> shrink (round (l * 100)) <*> shrink (round (a * 100)) <*> shrink (round (b * 100)))
