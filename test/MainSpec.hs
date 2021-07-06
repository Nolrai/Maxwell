module MainSpec (spec) where
import Test.Hspec
import Text.Printf
import Maxwell

showDigits :: Double -> String 
showDigits n = printf ("%" + show n + "f")

showXYZ :: (XYZ -> Double) -> String
showXYZ v = showDigits (v X) + ", " + showDigits (v Y) + ", " + showDigits (v Z)

spec :: Spec
spec =
  describe "okLabToXYZ" $
    describe "should match the examples from https://bottosson.github.io/posts/oklab/" $ do
      it "L 1 a* 0 b* 0" $
        showXYZ ()
      
      