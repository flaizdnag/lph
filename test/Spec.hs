import LogicPrograms
import TpOperator
import CPL
import Test.Hspec
{-
ex1 :: LogicP
ex1 = [ (A 1 [], [A 2 [], A 3 []], [A 4 []]),
        (A 5 [], [A 6 []], []),
        (A 10 [], [], []),
        (A 1 [], [], [])]
-}
main :: IO ()
main = hspec $ do
    describe "Tests for functions in CPL module" $ do
        it "isModel2vCPL checks if a given interpretation is a model for a given set of formulas (for a two-valued semantics)" $
            isModel2vCPL [E (V (A 1 "")) (C [V (A 1 ""), V (A 2 "")])] (IntCPL [V (A 1 ""), V (A 2 "")] []) `shouldBe` True

        it "isModel2vCPL checks if a given interpretation is a model for a given set of formulas (for a two-valued semantics)" $
            isModel2vCPL [E (V (A 1 "")) (D [(C [V (A 1 ""), V (A 2 "")]), V (A 3 "")])] (IntCPL [V (A 3 "")] [V (A 2 "")]) `shouldBe` False

        it "isModel3vCPL checks if a given interpretation is a model for a given set of formulas (for a three-valued semantics)" $
            isModel3vCPL [E (V (A 1 "")) (C [V (A 1 ""), V (A 2 "")])] (IntCPL [V (A 1 ""), V (A 2 "")] []) `shouldBe` True

        it "isModel2vCPL checks if a given interpretation is a model for a given set of formulas (for a two-valued semantics)" $
            isModel2vCPL [E (V (A 1 "")) (D [(C [V (A 1 ""), V (A 2 "")]), V (A 3 "")])] (IntCPL [V (A 3 "")] [V (A 2 "")]) `shouldBe` False
    