module PhiOperatorTests (phiOperatorTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import PhiOperator
import Test.Hspec
import Test.QuickCheck

phiOperatorTests :: IO ()
phiOperatorTests = hspec $ do
    describe "PhiOperator module; opPhi" $ do
        it "for lp1 and int1" $
            opPhi lp1 int1 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int2" $
            opPhi lp1 int2 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int3" $
            opPhi lp1 int3 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int4" $
            opPhi lp1 int4 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp1 and int5" $
            opPhi lp1 int5 `shouldBe` IntLP [A 1 "", A 2 ""] []

        it "for lp2 and int1" $
            opPhi lp2 int1 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp2 and int2" $
            opPhi lp2 int2 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp2 and int3" $
            opPhi lp2 int3 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp2 and int4" $
            opPhi lp2 int4 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp2 and int5" $
            opPhi lp2 int5 `shouldBe` IntLP [A 1 "", A 2 ""] []

        it "for lp3 and int1" $
            opPhi lp3 int1 `shouldBe` IntLP [A 3 ""] [A 4 ""]
        it "for lp3 and int2" $
            opPhi lp3 int2 `shouldBe` IntLP [A 3 ""] [A 4 ""]
        it "for lp3 and int3" $
            opPhi lp3 int3 `shouldBe` IntLP [A 2 "", A 3 ""] [A 4 ""]
        it "for lp3 and int4" $
            opPhi lp3 int4 `shouldBe` IntLP [A 3 ""] [A 1 "", A 2 "", A 4 ""]
        it "for lp3 and int5" $
            opPhi lp3 int5 `shouldBe` IntLP [A 3 ""] [A 2 "", A 4 ""]

        it "for lp4 and int1" $
            opPhi lp4 int1 `shouldBe` IntLP [] []
        it "for lp4 and int2" $
            opPhi lp4 int2 `shouldBe` IntLP [] []
        it "for lp4 and int3" $
            opPhi lp4 int3 `shouldBe` IntLP [] [A 1 ""]
        it "for lp4 and int4" $
            opPhi lp4 int4 `shouldBe` IntLP [] [A 1 ""]
        it "for lp4 and int5" $
            opPhi lp4 int5 `shouldBe` IntLP [A 1 ""] []

        it "for lp5 and int1" $
            opPhi lp5 int1 `shouldBe` IntLP [] []
        it "for lp5 and int2" $
            opPhi lp5 int2 `shouldBe` IntLP [] []
        it "for lp5 and int3" $
            opPhi lp5 int3 `shouldBe` IntLP [A 2 ""] []
        it "for lp5 and int4" $
            opPhi lp5 int4 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp5 and int5" $
            opPhi lp5 int5 `shouldBe` IntLP [A 2 ""] [A 1 ""]


    describe "PhiOperator module; iterOpPhi" $ do
        it "for lp1" $
            iterOpPhi lp1 `shouldBe` [IntLP [A 1 "", A 2 ""] [], int1]
        it "for lp2" $
            iterOpPhi lp2 `shouldBe` [IntLP [A 1 "", A 2 ""] [], int1]
        it "for lp3" $
            iterOpPhi lp3 `shouldBe` [IntLP [A 3 ""] [A 4 ""], int1]
        it "for lp4" $
            iterOpPhi lp4 `shouldBe` [int1]
        it "for lp5" $
            iterOpPhi lp5 `shouldBe` [int1]


    describe "PhiOperator module; findModelOpPhi" $ do
        it "for lp1" $
            findModelOpPhi lp1 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp2" $
            findModelOpPhi lp2 `shouldBe` IntLP [A 1 "", A 2 ""] []
        it "for lp3" $
            findModelOpPhi lp3 `shouldBe` IntLP [A 3 ""] [A 4 ""]
        it "for lp4" $
            findModelOpPhi lp4 `shouldBe` int1
        it "for lp5" $
            findModelOpPhi lp5 `shouldBe` int1


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
