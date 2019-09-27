module LvlMapTests (lvlMapTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import LvlMap
import Test.Hspec
import Test.QuickCheck

lvlMapTests :: IO ()
lvlMapTests = hspec $ do
    describe "LvlMap module; possibleLvLMaps" $ do
        it "for lp1" $
            length (possibleLvLMaps lp1) `shouldBe` 1
        it "for lp2" $
            length (possibleLvLMaps lp2) `shouldBe` 1
        it "for lp3" $
            length (possibleLvLMaps lp3) `shouldBe` 24
        it "for lp4" $
            length (possibleLvLMaps lp4) `shouldBe` 1
        it "for lp5" $
            length (possibleLvLMaps lp5) `shouldBe` 2
