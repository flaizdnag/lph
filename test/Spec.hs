import LogicPrograms
import TpOperator
import Test.Hspec

ex1 :: LP
ex1 = [ (A 1 "", [A 2 "", A 3 ""], [A 4 ""]),
        (A 5 "", [A 6 ""], []),
        (A 10 "", [], []),
        (A 1 "", [], [])]

main :: IO ()
main = hspec $ do
    describe "Tests for funcions in LogicPrograms module" $ do
        it "clHead returns head of Horn clause in a list" $
            clHead (A 1 "", [], []) `shouldBe` A 1 ""

        it "clBody returns body of a Horn clause without repetitions" $
            clBody (A 1 "", [A 1 "", A 2 ""], [A 2 ""]) `shouldBe` [A 1 "", A 2 ""]

        it "clPBody returns positive body of a Horn clause without repetitions" $
            clPBody (A 1 "", [A 1 "", A 2 ""], [A 2 ""]) `shouldBe` [A 1 "", A 2 ""]

        it "clNBody returns negative body of a Horn clause without repetitions" $
            clNBody (A 1 "", [A 1 "", A 2 ""], [A 2 ""]) `shouldBe` [A 2 ""]

        it "lpHeads returns all heads from a given LP without repetitions" $
            lpHeads [(A 1 "", [A 1 "", A 2 ""], [A 2 ""]), (A 1 "", [], [])] `shouldBe` [A 1 ""]

        it "lpBodies returns all atoms from the bodies of Horn clauses from a given LP without repetitions" $
            lpBodies [(A 1 "", [A 1 "", A 2 ""], [A 2 ""]), (A 1 "", [A 1 ""], [])] `shouldBe` [A 1 "", A 2 ""]

        it "lpPBodies returns all atoms from the positive bodies of Horn clauses from a given LP without repetitions" $
            lpPBodies [(A 1 "", [A 1 "", A 2 ""], [A 2 ""]), (A 1 "", [A 1 ""], [])] `shouldBe` [A 1 "", A 2 ""]

        it "bp returns all atoms occuring in a given LP without repetitions" $
            bp [(A 1 "", [A 1 "", A 2 ""], [A 2 ""]), (A 1 "", [A 1 ""], [])] `shouldBe` [A 1 "", A 2 ""]

{-    describe "Tests for funcions in TpOperator module" $ do
        it "opTp returns head of a clause that positive body is a subset of a list of atoms without duplicates" $
            opTp [(A 1 "", [A 1 "", A 2 ""], [A 3 ""]), (A 1 "", [A 1 ""], [])] [A 1 "", A 2 ""] `shouldBe` [A 1 ""]

        it "opTp returns head of a clause that positive body is a subset of a list of atoms without duplicates" $
            opTp [(A 1 "", [A 1 "", A 2 ""], [A 3 ""]), (A 1 "", [A 1 ""], [])] [A 1 "", A 2 ""] `shouldBe` [A 1 ""]

        it "upArrow returns model for acceptable LP (first item)" $
            head (upArrow [(A 1 "", [A 2 ""], []), (A 1 "", [A 3 ""], []), (A 3 "", [], []), (A 2 "", [], [])]) `shouldBe` [A 1 "", A 3 "", A 2 ""]

        it "upArrow returns model for acceptable LP (first item)" $
            head (upArrow [(A 1 "", [A 2 ""], []), (A 1 "", [A 3 ""], []), (A 3 "", [], []), (A 2 "", [], [])]) `shouldBe` [A 1 "", A 3 "", A 2 ""]

    describe "Tests for functions in CPL module" $ do
        it "eval2v returns the value of disjunction, where one of the elements is T" $
            eval2v (D [C [V (A 1 ""), N (V (A 2 ""))], T]) ([V (A 1 "")], []) `shouldBe` Tr-}
