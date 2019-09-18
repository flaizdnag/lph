module AcceptableTests (acceptableTests) where

import ExamplesToTest
import Acceptable
import Test.Hspec
import Test.QuickCheck


acceptableTests :: IO ()
acceptableTests = hspec $ do
    describe "Acceptable module; isAcceptable" $ do
        it "for lp1 it's true" $
            isAcceptable lp1 `shouldBe` True
        it "for lp2 it's true" $
            isAcceptable lp2 `shouldBe` True
        it "for lp3 it's true" $
            isAcceptable lp3 `shouldBe` True
        it "for lp4 it's true" $
            isAcceptable lp4 `shouldBe` True
        it "for lp5 it's false" $
            isAcceptable lp5 `shouldBe` False
