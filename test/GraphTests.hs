module GraphTests (graphTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import Graph
import Test.Hspec
import Test.QuickCheck

graphTests :: IO ()
graphTests = hspec $ do
    describe "Graph module; atomsToInts" $ do
        it "for [] it's empty" $
            atomsToInts [] `shouldBe` []
        it "for [A1, A2] it's [1,2]" $
            atomsToInts [A 1 "", A 2 ""] `shouldBe` [1,2]
    describe "Graph module; intsToAtoms" $ do
        it "for [] it's empty" $
            intsToAtoms [] `shouldBe` []
        it "for [1,2] it's [A1, A2]" $
            intsToAtoms [1,2] `shouldBe` [A 1 "", A 2 ""]
    describe "Graph module; limits" $ do
        it "for lp1" $
            limits lp1 `shouldBe` (1, 2)
        it "for lp2" $
            limits lp2 `shouldBe` (1, 16)
        it "for lp3" $
            limits lp3 `shouldBe` (1, 5)
        it "for lp4" $
            limits lp4 `shouldBe` (1, 2)
        it "for lp5" $
            limits lp5 `shouldBe` (1, 2)
    describe "Graph module; lpEdges" $ do
        it "for lp1" $
            lpEdges lp1 `shouldBe` []
        it "for lp2" $
            eqLists (lpEdges lp2) lp2Edges `shouldBe` True
        it "for lp3" $
            eqLists (lpEdges lp3) lp3Edges `shouldBe` True
        it "for lp4" $
            eqLists (lpEdges lp4) lp4Edges `shouldBe` True
        it "for lp5" $
            eqLists (lpEdges lp5) lp5Edges `shouldBe` True
    describe "Graph module; dependsOn" $ do
        it "for lp1 and 1" $
            dependsOn (graph lp1) 1 `shouldBe` []
        it "for lp2 and 1" $
            eqLists (dependsOn (graph lp2) 1) [10,11] `shouldBe` True
        it "for lp3 and 1" $
            eqLists (dependsOn (graph lp3) 1) [2,3,4,5,1] `shouldBe` True


lp2Edges :: [(Int, Int)]
lp2Edges =
    [ (1, 10), (1, 11)
    , (2, 15), (2, 16)
    ]


lp3Edges :: [(Int, Int)]
lp3Edges =
    [ (1, 2), (1, 3), (1, 4), (1, 5)
    , (2, 1), (2, 3), (2, 4), (2, 5)
    ]


lp4Edges :: [(Int, Int)]
lp4Edges = [ (1, 2), (1, 1) ]


lp5Edges :: [(Int, Int)]
lp5Edges = [ (2, 1), (1, 2), (2, 1) ]
