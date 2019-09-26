module LPsimplifierTests (lpSimplifierTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import LPsimplifier
import Test.Hspec
import Test.QuickCheck

lpSimplifierTests :: IO ()
lpSimplifierTests = hspec $ do
    describe "LPsimplifier module; simplifyLP" $ do
        it "for [A1 <- A2] nothing changes" $
            simplifyLP [Cl (A 1 "") [A 2 ""] []] `shouldBe` [Cl (A 1 "") [A 2 ""] []]
        it "for [A1 <- ~A2] nothing changes" $
            simplifyLP [Cl (A 1 "") [] [A 2 ""]] `shouldBe` [Cl (A 1 "") [] [A 2 ""]]
        it "for lp1" $
            eqLists (simplifyLP lp1) simpLP1 `shouldBe` True
        it "for lp2" $
            eqLists (simplifyLP lp2) simpLP2 `shouldBe` True
        it "for lp3" $
            eqLists (simplifyLP lp3) simpLP3 `shouldBe` True
        it "for lp4" $
            simplifyLP lp4 `shouldBe` lp4
        it "for lp5" $
            eqLists (simplifyLP lp5) simpLP5 `shouldBe` True


simpLP1 :: LP
simpLP1 = [ Fact (A 1 ""), Fact (A 2 "") ]


simpLP2 :: LP
simpLP2 = [ Fact (A 1 ""), Fact (A 2 "") ]


simpLP3 :: LP
simpLP3 =
    [ Cl (A 1 "") [A 2 "", A 3 ""] [A 4 "", A 5 ""]
    , Cl (A 2 "") [A 1 "", A 3 ""] [A 4 "", A 5 ""]
    , Fact (A 3 "")
    , Fact (A 4 "")
    ]


simpLP5 :: LP
simpLP5 = [ Fact (A 2 ""), Cl (A 1 "") [] [A 2 ""] ]
