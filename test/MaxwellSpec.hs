module MaxwellSpec (spec) where
import Test.Hspec
import Text.Printf
import Maxwell

showDigits :: Int -> Double -> String 
showDigits n = printf ("%." <> show n <> "f")

showXYZ :: (XYZ -> Double) -> String
showXYZ v = showDigits 3 (v X) <> ", " <> showDigits 3 (v Y) <> ", " <> showDigits 3 (v Z)

showLAB :: (Lab -> Double) -> String
showLAB lab = printf "L %f a* %f b* %f" (lab Light) (lab AStar) (lab BStar)

testCase :: (Lab -> Double) -> String -> Spec
testCase lab result = 
  it (showLAB lab)
    $ showXYZ (toXYZfromOK lab) `shouldBe` result

spec :: Spec
spec =
  describe "toXYZfromLMS . toLMSFromOk" $
    describe "should match the examples from https://bottosson.github.io/posts/oklab/" $ do
      testCase (toLab 1 0 0) "0.950, 1.000, 1.089"
