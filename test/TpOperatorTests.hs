module TpOperatorTests (tpOperatorTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import TpOperator
import Test.Hspec
import Test.QuickCheck

tpOperatorTests :: IO ()
tpOperatorTests = hspec $ do
    describe "PhiOperator module; opTp" $ do
        it "for lp1 and int1" $
            opTp lp1 int1 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int2" $
            opTp lp1 int2 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int3" $
            opTp lp1 int3 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int4" $
            opTp lp1 int4 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int5" $
            opTp lp1 int5 `shouldBe` IntLP [A 1 "", A 2 ""] []

        it "for lp2 and int1" $
            opTp lp2 int1 `shouldBe` IntLP [A 1 "", A 2 ""] [A 10 "", A 11 "", A 15 "", A 16 ""]
        it "for lp2 and int2" $
            opTp lp2 int2 `shouldBe` IntLP [A 1 "", A 2 ""] [A 10 "", A 11 "", A 15 "", A 16 ""]
        it "for lp2 and int3" $
            opTp lp2 int3 `shouldBe` IntLP [A 1 "", A 2 ""] [A 10 "", A 11 "", A 15 "", A 16 ""]
        it "for lp2 and int4" $
            opTp lp2 int4 `shouldBe` IntLP [A 1 "", A 2 ""] [A 10 "", A 11 "", A 15 "", A 16 ""]
        it "for lp2 and int5" $
            opTp lp2 int5 `shouldBe` IntLP [A 1 "", A 2 ""] [A 10 "", A 11 "", A 15 "", A 16 ""]

        it "for lp3 and int1" $
            opTp lp3 int1 `shouldBe` IntLP [A 3 ""] [A 1 "", A 2 "", A 4 "", A 5 ""]
        it "for lp3 and int2" $
            opTp lp3 int2 `shouldBe` IntLP [A 3 ""] [A 1 "", A 2 "", A 4 "", A 5 ""]
        it "for lp3 and int3" $
            opTp lp3 int3 `shouldBe` IntLP [A 2 "", A 3 ""] [A 1 "", A 4 "", A 5 ""]
        it "for lp3 and int4" $
            opTp lp3 int4 `shouldBe` IntLP [A 3 ""] [A 1 "", A 2 "", A 4 "", A 5 ""]
        it "for lp3 and int5" $
            opTp lp3 int5 `shouldBe` IntLP [A 3 ""] [A 1 "", A 2 "", A 4 "", A 5 ""]

        it "for lp4 and int1" $
            opTp lp4 int1 `shouldBe` IntLP [] [A 1 "", A 2 ""]
        it "for lp4 and int2" $
            opTp lp4 int2 `shouldBe` IntLP [] [A 1 "", A 2 ""]
        it "for lp4 and int3" $
            opTp lp4 int3 `shouldBe` IntLP [] [A 1 "", A 2 ""]
        it "for lp4 and int4" $
            opTp lp4 int4 `shouldBe` IntLP [] [A 1 "", A 2 ""]
        it "for lp4 and int5" $
            opTp lp4 int5 `shouldBe` IntLP [A 1 ""] [A 2 ""]

        it "for lp5 and int1" $
            opTp lp5 int1 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp5 and int2" $
            opTp lp5 int2 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp5 and int3" $
            opTp lp5 int3 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp5 and int4" $
            opTp lp5 int4 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp5 and int5" $
            opTp lp5 int5 `shouldBe` IntLP [A 2 ""] [A 1 ""]


    describe "PhiOperator module; upArrow" $ do
        it "for lp1" $
            upArrow lp1 `shouldBe` [IntLP [A 1 "", A 2 ""] [], int1]
        it "for lp2" $
            upArrow lp2 `shouldBe` [IntLP [A 1 "", A 2 ""] [A 10 "", A 11 "", A 15 "", A 16 ""], int1]
        it "for lp3" $
            upArrow lp3 `shouldBe` [IntLP [A 3 ""] [A 1 "", A 2 "", A 4 "", A 5 ""], int1]
        it "for lp4" $
            upArrow lp4 `shouldBe` [IntLP [] [A 1 "", A 2 ""], int1]
        it "for lp5" $
            upArrow lp5 `shouldBe` [IntLP [A 2 ""] [A 1 ""], IntLP [A 1 "", A 2 ""] [], int1]


    describe "PhiOperator module; isCondequenceA" $ do
        it "for lp1 and A1" $
            isConsequenceA (A 1 "") lp1 `shouldBe` True
        it "for lp2 and A2" $
            isConsequenceA (A 2 "") lp2 `shouldBe` True
        it "for lp3 and A1" $
            isConsequenceA (A 1 "") lp3 `shouldBe` False
        it "for lp4 and A1" $
            isConsequenceA (A 1 "") lp4 `shouldBe` False
        it "for lp5 and A1" $
            isConsequenceA (A 1 "") lp5 `shouldBe` False
        it "for lp5 and A2" $
            isConsequenceA (A 2 "") lp5 `shouldBe` True


int1 :: IntLP
int1 = IntLP [] []


int2 :: IntLP
int2 = IntLP [A 10 ""] [A 11 ""]


int3 :: IntLP
int3 = IntLP [A 1 "", A 3 ""] [A 4 "", A 5 ""]


int4 :: IntLP
int4 = IntLP [A 4 "", A 5 ""] [A 1 "", A 2 "", A 3 ""]


int5 :: IntLP
int5 = IntLP [A 2 ""] [A 1 ""]
