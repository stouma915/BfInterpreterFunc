module LibSpec( spec ) where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "Lib.evaluate" $
    it "evaluates code and return output" $
      evaluate "----[---->+<]>++.+.+.+." `shouldBe` Just("ABCD")
