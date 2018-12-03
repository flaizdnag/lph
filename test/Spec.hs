import LogicPrograms
import TpOperator
import CPL
import Test.Hspec

    
main :: IO ()
main = hspec $ do
    describe "Tests for funcions in LogicPrograms module" $ do
        it "clHead returns head of Horn clause in a list" $
            clHead (Cl (A 1 "") [] []) `shouldBe` A 1 ""

        it "clBody returns body of a Horn clause without repetitions" $
            clBody (Cl (A 1 "") [A 1 "", A 2 ""] [A 2 ""]) `shouldBe` [A 1 "", A 2 ""]

        it "clPBody returns positive body of a Horn clause without repetitions" $
            clPBody (Cl (A 1 "") [A 1 "", A 2 ""] [A 2 ""]) `shouldBe` [A 1 "", A 2 ""]

        it "clNBody returns negative body of a Horn clause without repetitions" $
            clNBody (Cl (A 1 "") [A 1 "", A 2 ""] [A 2 ""]) `shouldBe` [A 2 ""]

        it "lpHeads returns all heads from a given LP without repetitions" $
            lpHeads [Cl (A 1 "") [A 1 "", A 2 ""] [A 2 ""], Cl (A 1 "") [] []] `shouldBe` [A 1 ""]

        it "lpBodies returns all atoms from the bodies of Horn clauses from a given LP without repetitions" $
            lpBodies [Cl (A 1 "") [A 1 "", A 2 ""] [A 2 ""], Cl (A 1 "") [A 1 ""] []] `shouldBe` [A 1 "", A 2 ""]

        it "lpPBodies returns all atoms from the positive bodies of Horn clauses from a given LP without repetitions" $
            lpPBodies [Cl (A 1 "") [A 1 "", A 2 ""] [A 2 ""], Cl (A 1 "") [A 1 ""] []] `shouldBe` [A 1 "", A 2 ""]

        it "bp returns all atoms occuring in a given LP without repetitions" $
            bp [Cl (A 1 "") [A 1 "", A 2 ""] [A 2 ""], Cl (A 1 "") [A 1 ""] []] `shouldBe` [A 1 "", A 2 ""]

    describe "Tests for funcions in TpOperator module" $ do
        it "opTp returns head of a clause that positive body is a subset of a list of atoms without duplicates" $
            opTp ([Cl (A 1 "") [A 1 "", A 2 ""] [A 3 ""], Cl (A 1 "") [A 1 ""] []]) (IntLP [A 1 "", A 2 ""] [A 3 ""]) `shouldBe` IntLP [A 1 ""] [A 2 "", A 3 ""]

        it "opTp returns head of a clause that positive body is a subset of a list of atoms without duplicates" $
            opTp ([Cl (A 1 "") [A 1 "", A 2 ""] [A 3 ""], Cl (A 1 "") [A 1 ""] []]) (IntLP [] []) `shouldBe` IntLP [] [A 1 "", A 2 "", A 3 ""]

        it "upArrow returns model for acceptable LP (first item)" $
            head (upArrow [Cl (A 1 "") [A 2 ""] [], Cl (A 1 "") [A 3 ""] [], Cl (A 3 "") [] [], Cl (A 2 "") [] []]) `shouldBe` IntLP [A 1 "", A 3 "", A 2 ""] []

        it "upArrow returns model for acceptable LP (first item)" $
            head (upArrow [Cl (A 1 "") [A 2 ""] [A 1 ""], Cl (A 1 "") [A 1 ""] [], Cl (A 3 "") [] [], Cl (A 2 "") [] []]) `shouldBe` IntLP [A 3 "", A 2 ""] [A 1 ""]

    describe "Tests for functions in CPL module" $ do
        it "isModel2vCPL checks if a given interpretation is a model for a given set of formulas (for a two-valued semantics)" $
            isModel2vCPL [E (V (A 1 "")) (C [V (A 1 ""), V (A 2 "")])] (IntCPL [V (A 1 ""), V (A 2 "")] []) `shouldBe` True

        it "isModel2vCPL checks if a given interpretation is a model for a given set of formulas (for a two-valued semantics)" $
            isModel2vCPL [E (V (A 1 "")) (D [(C [V (A 1 ""), V (A 2 "")]), V (A 3 "")])] (IntCPL [V (A 3 "")] [V (A 2 "")]) `shouldBe` False

        it "isModel3vCPL checks if a given interpretation is a model for a given set of formulas (for a three-valued semantics)" $
            isModel3vCPL [E (V (A 1 "")) (C [V (A 1 ""), V (A 2 "")])] (IntCPL [V (A 1 ""), V (A 2 "")] []) `shouldBe` True

        it "isModel2vCPL checks if a given interpretation is a model for a given set of formulas (for a two-valued semantics)" $
            isModel2vCPL [E (V (A 1 "")) (D [(C [V (A 1 ""), V (A 2 "")]), V (A 3 "")])] (IntCPL [V (A 3 "")] [V (A 2 "")]) `shouldBe` False
