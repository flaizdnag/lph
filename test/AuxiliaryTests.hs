module AuxiliaryTests (auxiliaryTests) where

import Auxiliary
import ExamplesToTest
import Test.Hspec
import Test.QuickCheck


auxiliaryTests :: IO ()
auxiliaryTests = hspec $ do
    describe "Auxiliary module; isSublist" $ do
        it "for two empty lists it's true" $
            twoEmptySubLists `shouldBe` True
        it "for [1,2] and [1,2] it's true" $
            isSublist [1,2] [1,2] `shouldBe` True
        it "for [1,2] and [1,2,3] it's true" $
            isSublist [1,2] [1,2,3] `shouldBe` True
        it "for [1,2] and [1] it's false" $
            isSublist [1,2] [1] `shouldBe` False
    describe "Auxiliary module; isProperSublist" $ do
        it "for two empty lists it's false" $
            twoEmptyPropSubLists `shouldBe` False
        it "for [1,2] and [1,2] it's false" $
            isProperSublist [1,2] [1,2] `shouldBe` False
        it "for [1,2] and [1,2,3] it's true" $
            isProperSublist [1,2] [1,2,3] `shouldBe` True
        it "for [1,2] and [1] it's false" $
            isProperSublist [1,2] [1] `shouldBe` False
    describe "Auxiliary module; jointElem" $ do
        it "for two empty lists it's false" $
            twoEmptyJointElem `shouldBe` False
        it "for [1,2] and [1,2] it's true" $
            jointElem [1,2] [1,2] `shouldBe` True
        it "for [1,2] and [1,2,3] it's true" $
            jointElem [1,2] [1,2,3] `shouldBe` True
        it "for [1,2] and [1] it's true" $
            jointElem [1,2] [1] `shouldBe` True
        it "for [1,2] and [3,4] it's false" $
            jointElem [1,2] [3,4] `shouldBe` False
    describe "Auxiliary module; eqLists" $ do
        it "for two empty lists it's true" $
            twoEmptyEqLists `shouldBe` True
        it "for [1,2] and [1,2] it's true" $
            eqLists [1,2] [1,2] `shouldBe` True
        it "for [1,2] and [1,2,3] it's false" $
            eqLists [1,2] [1,2,3] `shouldBe` False
        it "for [1,2] and [1] it's false" $
            eqLists [1,2] [1] `shouldBe` False
        it "for [1,2] and [2,1] it's true" $
            eqLists [1,2] [2,1] `shouldBe` True
    describe "Auxiliary module; ltLists" $ do
        it "for two empty lists it's false" $
            twoEmptyLtLists `shouldBe` False
        it "for [1,2] and [1,2] it's false" $
            ltLists [1,2] [1,2] `shouldBe` False
        it "for [1,2] and [1,2,3] it's True" $
            ltLists [1,2] [1,2,3] `shouldBe` True
        it "for [1,2] and [1] it's false" $
            ltLists [1,2] [1] `shouldBe` False
        it "for [1,2] and [2,1] it's false" $
            ltLists [1,2] [2,1] `shouldBe` False
        it "for [1,2] and [3,2,1] it's true" $
            ltLists [1,2] [3,2,1] `shouldBe` True
    describe "Auxiliary module; symDifference" $ do
        it "for two empty lists it's empty" $
            twoEmptySymDiff `shouldBe` []
        it "for [1,2] and [1,2] it's empty" $
            symDifference [1,2] [1,2] `shouldBe` []
        it "for [1,2] and [1,2,3] it's [3]" $
            symDifference [1,2] [1,2,3] `shouldBe` [3]
        it "for [1,2] and [1] it's [2]" $
            symDifference [1,2] [1] `shouldBe` [2]
        it "for [1,2] and [2,1] it's empty" $
            symDifference [1,2] [2,1] `shouldBe` []
        it "for [1,2] and [4,3] it's fst ++ snd" $
            eqLists (symDifference [1,2] [4,3]) [1,2,3,4] `shouldBe` True
    describe "Auxiliary module; uniquePairs" $ do
        it "for empty list it's empty" $
            emptyUniquePairs `shouldBe` []
        it "for [1,2,3] it's [(1,2),(1,3),(2,3)]" $
            uniquePairs [1,2,3] `shouldBe` [(1,2),(1,3),(2,3)]


twoEmptySubLists :: Bool
twoEmptySubLists = isSublist xs ys
    where
        xs = [] :: [Int]
        ys = [] :: [Int]


twoEmptyPropSubLists :: Bool
twoEmptyPropSubLists = isProperSublist xs ys
    where
        xs = [] :: [Int]
        ys = [] :: [Int]


twoEmptyJointElem :: Bool
twoEmptyJointElem = jointElem xs ys
    where
        xs = [] :: [Int]
        ys = [] :: [Int]


twoEmptyEqLists :: Bool
twoEmptyEqLists = eqLists xs ys
    where
        xs = [] :: [Int]
        ys = [] :: [Int]


twoEmptyLtLists :: Bool
twoEmptyLtLists = ltLists xs ys
    where
        xs = [] :: [Int]
        ys = [] :: [Int]


twoEmptySymDiff :: [Int]
twoEmptySymDiff = symDifference xs ys
    where
        xs = [] :: [Int]
        ys = [] :: [Int]


emptyUniquePairs :: [(Int, Int)]
emptyUniquePairs = uniquePairs xs
    where
        xs = [] :: [Int]
