import LogicPrograms
import TpOperator
import Test.Hspec

ex1 :: LP
ex1 = [ (Cl (A 1 "") [A 2 "", A 3 ""] [A 4 ""]),
        (Cl (A 5 "") [A 6 ""] []),
        (Cl (A 10 "") [] []),
        (Cl (A 1 "") [] [])]

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

    --describe "Tests for functions in CPL module" $ do
      --  it "eval2v returns the value of disjunction, where one of the elements is T" $
        --    eval2v (D [C [V (A 1 ""), N (V (A 2 ""))], T]) ([V (A 1 "")], []) `shouldBe` Tr
