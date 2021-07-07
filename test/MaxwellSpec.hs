module MaxwellSpec (spec) where
import Test.Hspec ( describe, it, shouldBe, Spec )
import Text.Printf ( printf )
import Maxwell
import Linear.V3 (V3(V3))
import Control.Lens

showDigits :: Int -> Double -> String 
showDigits n = printf ("%." <> show n <> "f")

showXYZ :: V3 Double -> String
showXYZ v = showDigits 3 (v ^. _1) <> ", " <> showDigits 3 (v ^. _2) <> ", " <> showDigits 3 (v ^. _3)

showLAB :: V3 Double -> String
showLAB lab = printf "L %f a* %f b* %f" (lab ^. _1) (lab ^. _2) (lab ^. _3)

testCase :: V3 Double -> String -> Spec
testCase lab result = 
  it (showLAB lab)
    $ showXYZ (toXYZfromOK lab) `shouldBe` result

spec :: Spec
spec =
  describe "toXYZfromLMS . toLMSFromOk" $
    describe "should match the examples from https://bottosson.github.io/posts/oklab/" $ do
      testCase (V3 1     0          0)      "0.950, 1.000, 1.088"
      testCase (V3 0.450 1.236    (-0.019)) "1.001, -0.000, -0.000"
      testCase (V3 0.922 (-0.671)   0.263)  "0.000, 1.001, 0.001"
      testCase (V3 0.153 (-1.415) (-0.449)) "0.001, 0.000, 1.002"

