import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "Lib.evaluate" $ do
    it "evaluates code and return output" $ do
      evaluate "----[---->+<]>++.+.+.+." `shouldBe` Just("ABCD")
