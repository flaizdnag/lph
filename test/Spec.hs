import Formulas
import Operator
import Completion
import Test.Hspec

ex1 :: LogicP
ex1 = [ (A 1, [A 2, A 3], [A 4]),
        (A 5, [A 6], []),
        (A 10, [], []),
        (A 1, [], [])]

main :: IO ()
main = hspec $ do
    describe "Tests for funcions in Formulas module" $ do
        it "hClHead returns head of Horn clause in a list" $
            hClHead (A 1, [], []) `shouldBe` [A 1]

        it "hClBody returns body of a Horn clause without repetitions" $
            hClBody (A 1, [A 1, A 2], [A 2]) `shouldBe` [A 1, A 2]

        it "hClBodyP returns positive body of a Horn clause without repetitions" $
            hClBodyP (A 1, [A 1, A 2], [A 2]) `shouldBe` [A 1, A 2]

        it "hClBodyN returns positive body of a Horn clause without repetitions" $
            hClBodyN (A 1, [A 1, A 2], [A 2]) `shouldBe` [A 2]

        it "bPHead returns all heads from a given LP without repetitions" $
            bPHead [(A 1, [A 1, A 2], [A 2]), (A 1, [], [])] `shouldBe` [A 1]

        it "bPHead' returns all heads from a given LP without repetitions" $
            bPHead' [(A 1, [A 1, A 2], [A 2]), (A 1, [A 1], [])] `shouldBe` [A 1]

        it "bPBody returns all atoms from the bodies of Horn clauses from a given LP without repetitions" $
            bPBody [(A 1, [A 1, A 2], [A 2]), (A 1, [A 1], [])] `shouldBe` [A 1, A 2]

        it "bPBody' returns all atoms from the bodies of Horn clauses from a given LP without repetitions" $
            bPBody' [(A 1, [A 1, A 2], [A 2]), (A 1, [A 1], [])] `shouldBe` [A 1, A 2]

        it "bPBodyP returns all atoms from the positive bodies of Horn clauses from a given LP without repetitions" $
            bPBodyP [(A 1, [A 1, A 2], [A 2]), (A 1, [A 1], [])] `shouldBe` [A 1, A 2]

        it "bPBodyP' returns all atoms from the positive bodies of Horn clauses from a given LP without repetitions" $
            bPBodyP' [(A 1, [A 1, A 2], [A 2]), (A 1, [A 1], [])] `shouldBe` [A 1, A 2]

        it "bP returns all atoms occuring in a given LP without repetitions" $
            bP [(A 1, [A 1, A 2], [A 2]), (A 1, [A 1], [])] `shouldBe` [A 1, A 2]

    describe "Tests for funcions in Operator module" $ do
        it "opTp returns head of a clause that positive body is a subset of a list of atoms without duplicates" $
            opTp [(A 1, [A 1, A 2], [A 3]), (A 1, [A 1], [])] [A 1, A 2] `shouldBe` [A 1]

        it "upArrow returns model for acceptable LP" $
            upArrow [(A 1, [A 2], []), (A 1, [A 3], []), (A 3, [], []), (A 2, [], [])] `shouldBe` [A 1, A 3, A 2]

        it "opTp' returns head of a clause that positive body is a subset of a list of atoms without duplicates" $
            opTp' [(A 1, [A 1, A 2], [A 3]), (A 1, [A 1], [])] [A 1, A 2] `shouldBe` [A 1]

        it "upArrow' returns model for acceptable LP" $
            upArrow' [(A 1, [A 2], []), (A 1, [A 3], []), (A 3, [], []), (A 2, [], [])] `shouldBe` [A 1, A 3, A 2]
    describe "Tests for functions in Completion module" $ do
        it "trueD' returns the value of disjunction, where one of the elements is T" $
            trueD' (D [C [V (A 1), N (V (A 2))], T]) ([V (A 1)], []) `shouldBe` Tr
