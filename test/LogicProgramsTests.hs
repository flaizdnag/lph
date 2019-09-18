module LogicProgramsTests (lpTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import TwoValuedSem
import ThreeValuedSem
import Test.Hspec
import Test.QuickCheck

lpTests :: IO ()
lpTests = hspec $ do
    describe "LogicPrograms module; clHead" $ do
        it "for clause" $
            clHead (Cl (A 1 "") [A 2 ""] [A 3 ""]) `shouldBe` A 1 ""
        it "for fact" $
            clHead (Fact (A 1 "")) `shouldBe` A 1 ""
        it "for assumption" $
            clHead (Assumption (A 1 "")) `shouldBe` A 1 ""

    describe "LogicPrograms module; clBody" $ do
        it "returns whole body of a clause without repetitions" $
            clBody (Cl (A 1 "") [A 1 "", A 2 "", A 2 ""] [A 2 ""]) `shouldBe` [A 1 "", A 2 ""]
        it "returns empty list for facts" $
            clBody (Fact (A 1 "")) `shouldBe` []
        it "returns empty list for assumptions" $
            clBody (Assumption (A 1 "")) `shouldBe` []

    describe "LogicPrograms module; clPBody" $ do
        it "returns positive body of a clause without repetitions" $
            clPBody (Cl (A 1 "") [A 1 "", A 2 "", A 2 ""] [A 2 ""]) `shouldBe` [A 1 "", A 2 ""]
        it "returns empty list for facts" $
            clPBody (Fact (A 1 "")) `shouldBe` []
        it "returns empty list for assumptions" $
            clPBody (Assumption (A 1 "")) `shouldBe` []

    describe "LogicPrograms module; clNBody" $ do
        it "returns positive body of a clause without repetitions" $
            clNBody (Cl (A 1 "") [A 1 "", A 2 "", A 2 ""] [A 2 "", A 2 ""]) `shouldBe` [A 2 ""]
        it "returns empty list for facts" $
            clNBody (Fact (A 1 "")) `shouldBe` []
        it "returns empty list for assumptions" $
            clNBody (Assumption (A 1 "")) `shouldBe` []

    describe "LogicPrograms module; lpHeads" $ do
        it "returns all heads from lp1 without repetitions" $
            eqLists (lpHeads lp1) [A 1 "", A 2 ""] `shouldBe` True
        it "returns all heads from lp2 without repetitions" $
            eqLists (lpHeads lp2) [A 1 "", A 2 ""] `shouldBe` True

    describe "LogicPrograms module; lpBodies" $ do
        it "returns empty list for lp1" $
            lpBodies lp1 `shouldBe` []
        it "for lp2" $
            eqLists (lpBodies lp2) [A 11 "", A 10 "", A 15 "", A 16 ""] `shouldBe` True

    describe "LogicPrograms module; onlyHeads" $ do
        it "for lp1" $
            eqLists (onlyHeads lp1) [A 1 "", A 2 ""] `shouldBe` True
        it "for lp2" $
            eqLists (onlyHeads lp2) [A 1 "", A 2 ""] `shouldBe` True
        it "returns empty list for lp3" $
            onlyHeads lp3 `shouldBe` []

    describe "LogicPrograms module; onlyBodies" $ do
        it "returns empty list for lp1" $
            onlyBodies lp1 `shouldBe` []
        it "for lp2" $
            eqLists (onlyBodies lp2) [A 11 "", A 10 "", A 15 "", A 16 ""] `shouldBe` True
        it "for lp3" $
            onlyBodies lp3 `shouldBe` [A 5 ""]

    describe "LogicPrograms module; lpPBodies" $ do
        it "returns empty list for lp1" $
            lpPBodies lp1 `shouldBe` []
        it "for lp2" $
            eqLists (lpPBodies lp2) [A 10 "", A 15 ""] `shouldBe` True
        it "for lp3" $
            eqLists (lpPBodies lp3) [A 1 "", A 2 "", A 3 ""] `shouldBe` True

    describe "LogicPrograms module; lpNBodies" $ do
        it "returns empty list for lp1" $
            lpNBodies lp1 `shouldBe` []
        it "for lp2" $
            eqLists (lpNBodies lp2) [A 11 "", A 16 ""] `shouldBe` True
        it "for lp3" $
            eqLists (lpNBodies lp3) [A 4 "", A 5 ""] `shouldBe` True

    describe "LogicPrograms module; bp" $ do
        it "for lp1 bp = lpHeads" $
            bp lp1 `shouldBe` lpHeads lp1
        it "for lp2" $
            eqLists (bp lp2) [A 1 "", A 2 "", A 10 "", A 11 "", A 15 "", A 16 ""] `shouldBe` True
        it "for lp3" $
            eqLists (bp lp3) [A 1 "", A 2 "", A 3 "", A 4 "", A 5 ""] `shouldBe` True

    describe "LogicPrograms module; atomDef" $ do
        it "for lp1 and A1" $
            length (atomDef (A 1 "") lp1) `shouldBe` 2
        it "for lp2 and A1" $
            length (atomDef (A 1 "") lp2) `shouldBe` 3
        it "for lp3 and A10" $
            atomDef (A 10 "") lp3 `shouldBe` []

    describe "LogicPrograms module; evalBody2v" $ do
        it "for clause; true body" $
            evalBody2v (Cl (A 1 "") [A 2 ""] [A 3 ""]) (IntLP [A 2 ""] [A 3 ""]) `shouldBe` Tr2v
        it "for clause; false body" $
            evalBody2v (Cl (A 1 "") [A 2 ""] [A 3 ""]) (IntLP [] [A 1 "", A 2 ""]) `shouldBe` Fa2v
        it "for fact always true" $
            evalBody2v (Fact (A 1 "")) (IntLP [A 1 ""] [A 2 ""]) `shouldBe` Tr2v
        it "for assumption always false" $
            evalBody2v (Assumption (A 1 "")) (IntLP [A 1 ""] [A 2 ""]) `shouldBe` Fa2v

    describe "LogicPrograms module; evalBodyLukasiewicz" $ do
        it "for clause; true body" $
            evalBodyLukasiewicz (Cl (A 1 "") [A 2 ""] [A 3 ""]) (IntLP [A 2 ""] [A 3 ""]) `shouldBe` Tr3v
        it "for clause; false body" $
            evalBodyLukasiewicz (Cl (A 1 "") [A 2 ""] [A 3 ""]) (IntLP [] [A 1 "", A 2 ""]) `shouldBe` Fa3v
        it "for clause; undefined body" $
            evalBodyLukasiewicz (Cl (A 1 "") [A 2 ""] [A 3 ""]) (IntLP [] [A 1 "", A 3 ""]) `shouldBe` Un3v
        it "for fact always true" $
            evalBodyLukasiewicz (Fact (A 1 "")) (IntLP [A 1 ""] [A 2 ""]) `shouldBe` Tr3v
        it "for assumption always false" $
            evalBodyLukasiewicz (Assumption (A 1 "")) (IntLP [A 1 ""] [A 2 ""]) `shouldBe` Fa3v

    describe "LogicPrograms module; isModel2vLP" $ do
        it "model for lp1" $
            isModel2vLP lp1 (IntLP [A 1 "", A 2 ""] []) `shouldBe` True
        it "not a model for lp1" $
            isModel2vLP lp1 (IntLP [] [A 1 "", A 2 ""]) `shouldBe` False
        it "model for lp2" $
            isModel2vLP lp2 (IntLP [A 1 "", A 2 ""] []) `shouldBe` True
        it "not a model for lp2" $
            isModel2vLP lp2 (IntLP [A 1 "", A 15 ""] []) `shouldBe` False
        it "model for lp3" $
            isModel2vLP lp3 (IntLP [A 3 ""] []) `shouldBe` True
        it "not a model for lp3" $
            isModel2vLP lp3 (IntLP [A 2 "", A 3 ""] []) `shouldBe` False

    describe "LogicPrograms module; isModelLukasiewiczLP" $ do
        it "model for lp1" $
            isModelLukasiewiczLP lp1 (IntLP [A 1 "", A 2 ""] []) `shouldBe` True
        it "not a model for lp1" $
            isModelLukasiewiczLP lp1 (IntLP [] []) `shouldBe` False
        it "model for lp2" $
            isModelLukasiewiczLP lp2 (IntLP [A 1 "", A 2 ""] []) `shouldBe` True
        it "not a model for lp2" $
            isModelLukasiewiczLP lp2 (IntLP [A 1 "", A 15 ""] []) `shouldBe` False
        it "model for lp3" $
            isModelLukasiewiczLP lp3 (IntLP [A 3 ""] []) `shouldBe` True
        it "not a model for lp3" $
            isModelLukasiewiczLP lp3 (IntLP [A 2 "", A 3 ""] [A 1 ""]) `shouldBe` False

    describe "LogicPrograms module; intsLPgenerator2v" $ do
        it "for [A1, A2, A3] there are 8" $
            length (intsLPgenerator2v [A 1 "", A 2 "", A 3 ""]) `shouldBe` 8

    describe "LogicPrograms module; intsLPgeneratorLuk" $ do
        it "for [A1, A2, A3] there are 27" $
            length (intsLPgeneratorLuk [A 1 "", A 2 "", A 3 ""]) `shouldBe` 27

    describe "LogicPrograms module; getCounterModels2v" $ do
        it "for lp1 and 'A1 <- Top'" $
            getCounterModels2v lp1 (Fact (A 1 "")) `shouldBe` []
        it "for lp1 and 'A3 <- A4'" $
            null (getCounterModels2v lp1 (Cl (A 3 "") [A 4 ""] [])) `shouldBe` False
        it "for lp2 and 'A1 <- A3'" $
            getCounterModels2v lp2 (Cl (A 1 "") [A 3 ""] []) `shouldBe` []
        it "for lp2 and 'A3 <- A4'" $
            null (getCounterModels2v lp2 (Cl (A 3 "") [A 4 ""] [])) `shouldBe` False
        it "for lp3 and 'A3 <- A4'" $
            getCounterModels2v lp3 (Cl (A 3 "") [A 4 ""] []) `shouldBe` []
        it "for lp3 and 'A2 <- ~A10'" $
            null (getCounterModels2v lp3 (Cl (A 2 "") [] [A 10 ""])) `shouldBe` False

    describe "LogicPrograms module; getCounterModelsLuk" $ do
        it "for lp1 and 'A1 <- Top'" $
            getCounterModelsLuk lp1 (Fact (A 1 "")) `shouldBe` []
        it "for lp1 and 'A3 <- A4'" $
            null (getCounterModelsLuk lp1 (Cl (A 3 "") [A 4 ""] [])) `shouldBe` False
        it "for lp2 and 'A1 <- A3'" $
            getCounterModelsLuk lp2 (Cl (A 1 "") [A 3 ""] []) `shouldBe` []
        it "for lp2 and 'A3 <- A4'" $
            null (getCounterModelsLuk lp2 (Cl (A 3 "") [A 4 ""] [])) `shouldBe` False
        it "for lp3 and 'A3 <- A4'" $
            getCounterModelsLuk lp3 (Cl (A 3 "") [A 4 ""] []) `shouldBe` []
        it "for lp3 and 'A2 <- ~A10'" $
            null (getCounterModelsLuk lp3 (Cl (A 2 "") [] [A 10 ""])) `shouldBe` False

    describe "LogicPrograms module; lpSymDifference" $ do
        it "for lp1 and lp2 there are 2" $
            length (lpSymDifference lp1 lp2) `shouldBe` 2
        it "for [] and lp1 it's lp1" $
            lpSymDifference [] lp1 `shouldBe` lp1
        it "for [A1^h <- Top] and lp1 it's lp1" $
            lpSymDifference [Fact (A 1 "h")] lp1 `shouldBe` lp1

    describe "LogicPrograms module; modifiedLP" $ do
        it "for lp1 and 'A1 <- Top' it's lp1" $
            modifiedLP lp1 (Fact (A 1 "")) `shouldBe` lp1
        it "for lp1 and 'A1 <- Bot' it's lp1" $
            modifiedLP lp1 (Assumption (A 1 "")) `shouldBe` lp1
        it "for lp1 and 'A1 <- A2, ~A3' it's [A2^h <- Top; A3^h <- Bot]" $
            eqLists (modifiedLP lp1 (Cl (A 1 "") [A 2 ""] [A 3 ""])) (lp1 ++ [Fact (A 2 "h"), Assumption (A 3 "h")]) `shouldBe` True

    describe "LogicPrograms module; overlappingAtoms" $ do
        it "for lp1 and empty list it's empty list" $
            overlappingAtoms lp1 [] `shouldBe` []
        it "for lp1 and [A1^h, A2^h] it's empty list" $
            overlappingAtoms lp1 [A 1 "h", A 2 "h"] `shouldBe` []
        it "for lp1 and [A1^hn, A2^hn] it's empty list" $
            overlappingAtoms lp1 [A 1 "hn", A 2 "hn"] `shouldBe` []
        it "for lp1 modified with 'A1 <- A2, A3^n' and [A3^n] there are two pairs" $
            eqLists (overlappingAtoms (modifiedLP lp1 (Cl (A 1 "") [A 2 "", A 3 "n"] [])) [A 3 "n"]) [(A 2 "", A 2 "h"), (A 3 "n", A 3 "nh")] `shouldBe` True

    describe "LogicPrograms module; bodyLength" $ do
        it "for facts it's 0" $
            bodyLength (Fact (A 1 "")) `shouldBe` 0
        it "for assumptions it's 0" $
            bodyLength (Assumption (A 1 "")) `shouldBe` 0
        it "for 'A1 <- A2, A3^h, ~A4, ~A4^h' it's 4" $
            bodyLength (Cl (A 1 "") [A 2 "", A 3 "h"] [A 4 "", A 4 "h"]) `shouldBe` 4
        it "for 'A1 <- A2, A2, ~A2, ~A2' it's 1" $
            bodyLength (Cl (A 1 "") [A 2 "", A 2 ""] [A 2 "", A 2 ""]) `shouldBe` 1

    describe "LogicPrograms module; bodiesLength" $ do
        it "for lp1 it's [0, 0, 0, 0]" $
            bodiesLength lp1 `shouldBe` [0,0,0,0]
        it "for lp2 it's 4 x 0 and 2 x 2" $
            eqLists (bodiesLength lp2) [0,0,0,0,2,2] `shouldBe` True
        it "for lp3 it's 2 x 4 and 2 x 0" $
            eqLists (bodiesLength lp3) [0,0,4,4] `shouldBe` True

    describe "LogicPrograms module; clSameHeads" $ do
        it "for 'A1 <- Top' and lp1 it's 2" $
            clSameHeads (Fact (A 1 "")) lp1 `shouldBe` 2
        it "for 'A1 <- Bot' and lp1 it's 2" $
            clSameHeads (Assumption (A 1 "")) lp1 `shouldBe` 2
        it "for 'A1^h <- Bot' and lp1 it's 0" $
            clSameHeads (Assumption (A 1 "h")) lp1 `shouldBe` 0

    describe "LogicPrograms module; clsSameHeads" $ do
        it "for lp1 it's 4 x 2" $
            clsSameHeads lp1 `shouldBe` [2,2,2,2]
        it "for lp2 it's 6 x 3" $
            clsSameHeads lp2 `shouldBe` [3,3,3,3,3,3]
        it "for lp3 it's 4 x 1" $
            clsSameHeads lp3 `shouldBe` [1,1,1,1]
