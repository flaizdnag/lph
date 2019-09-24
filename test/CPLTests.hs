module CPLTests (cplTests) where

import ExamplesToTest
import Auxiliary
import CPL
import LogicPrograms
import TwoValuedSem
import ThreeValuedSem
import Test.Hspec
import Test.QuickCheck


cplTests :: IO ()
cplTests = hspec $ do
    describe "CPL module; isModel2vCPL" $ do
        it "for ([], []) and [F] it's false" $
            isModel2vCPL [F] (IntCPL [] []) `shouldBe` False
        it "for ([], []) and [T] it's true" $
            isModel2vCPL [T] (IntCPL [] []) `shouldBe` True
        it "for ([], []) and forms1 it's false" $
            isModel2vCPL forms1 (IntCPL [] []) `shouldBe` False
        it "for ([A1, A3, A5], []) and forms1 it's false" $
            isModel2vCPL forms1 int1 `shouldBe` False
        it "for ([A1, A3, A5, A9], []) and forms1 it's true" $
            isModel2vCPL forms1 int2 `shouldBe` True
    
    describe "CPL module; isModelLukasiewiczCPL" $ do
        it "for ([], []) and [F] it's false" $
            isModelLukasiewiczCPL [F] (IntCPL [] []) `shouldBe` False
        it "for ([], []) and [T] it's true" $
            isModelLukasiewiczCPL [T] (IntCPL [] []) `shouldBe` True
        it "for ([], []) and forms1 it's false" $
            isModelLukasiewiczCPL forms1 (IntCPL [] []) `shouldBe` False
        it "for ([A1, A3, A5], []) and forms1 it's false" $
            isModelLukasiewiczCPL forms1 int1 `shouldBe` False
        it "for ([A1, A3, A5, A9], []) and forms1 it's true" $
            isModelLukasiewiczCPL forms1 int2 `shouldBe` True


int1 :: IntCPL
int1 = IntCPL [V (A 1 ""), V (A 3 ""), V (A 5 "")] []


int2 :: IntCPL
int2 = IntCPL [V (A 1 ""), V (A 3 ""), V (A 5 ""), V (A 9 "")] [V (A 2 ""), V (A 4 ""), V (A 6 ""), V (A 10 "")]
