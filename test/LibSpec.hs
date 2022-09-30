module LibSpec( spec ) where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "Lib.evaluate" $
    it "evaluates code and return output" $ do
      evaluate "----[---->+<]>++.+.+.+." `shouldBe` Just("ABCD")
      evaluate "-[----->+<]>--.+.+.+.+.+.------." `shouldBe` Just("1234560")
      evaluate "-[------->+<]>-.[--->++++<]>+.--[--->+<]>--.--------.------.+++++++.." `shouldBe` Just("Haskell")
      evaluate "[" `shouldBe` Nothing
