module CompletionTests (completionTests) where

import Completion
import CPL
import LogicPrograms
import Auxiliary
import ExamplesToTest
import Test.Hspec
import Test.QuickCheck

completionTests :: IO ()
completionTests = hspec $ do
    describe "Completion module; comp" $ do
        it "for lp1" $
            eqLists (comp lp1) compLP1 `shouldBe` True
        it "for lp2" $
            eqLists (comp lp2) compLP2 `shouldBe` True
        it "for lp3" $
            eqLists (comp lp3) compLP3 `shouldBe` True
        it "for lp4" $
            eqLists (comp lp4) compLP4 `shouldBe` True
        it "for lp5" $
            eqLists (comp lp5) compLP5 `shouldBe` True
    
    describe "Completion module; weakComp" $ do
        it "for lp1" $
            eqLists (weakComp lp1) weakCompLP1 `shouldBe` True
        it "for lp2" $
            eqLists (weakComp lp2) weakCompLP2 `shouldBe` True
        it "for lp3" $
            eqLists (weakComp lp3) weakCompLP3 `shouldBe` True
        it "for lp4" $
            eqLists (weakComp lp4) weakCompLP4 `shouldBe` True
        it "for lp5" $
            eqLists (weakComp lp5) weakCompLP5 `shouldBe` True
    
    describe "Completion module; intLPtoOntCPL" $ do
        it "for ([] ,[]) it's empty interpretation" $
            intLPtoIntCPL (IntLP [] []) `shouldBe` IntCPL [] []
        it "for ([A1] ,[A1])" $
            intLPtoIntCPL (IntLP [A 1 ""] [A 2 ""]) `shouldBe` IntCPL [V (A 1 "")] [V (A 2 "")]
        it "for ([A1, A2] ,[])" $
            intLPtoIntCPL (IntLP [A 1 "", A 2 ""] []) `shouldBe` IntCPL [V (A 1 ""), V (A 2 "")] []


weakCompLP1 :: [Form]
weakCompLP1 =
    [ E (V (A 1 "")) (D [C [T], C [F]])
    , E (V (A 2 "")) (D [C [T], C [F]])
    ]


compLP1 :: [Form]
compLP1 = weakCompLP1


weakCompLP2 :: [Form]
weakCompLP2 =
    [ E (V (A 1 "")) (D [C [V (A 10 ""), N (V (A 11 ""))], C [T], C [F]])
    , E (V (A 2 "")) (D [C [V (A 15 ""), N (V (A 16 ""))], C [T], C [F]])
    ]

compLP2 :: [Form]
compLP2 = weakCompLP2 ++
    [ N (V (A 10 "")), N (V (A 11 "")), N (V (A 15 "")), N (V (A 16 "")) ]


weakCompLP3 :: [Form]
weakCompLP3 =
    [ E (V (A 1 "")) (D [C [V (A 2 ""), V (A 3 ""), N (V (A 4 "")), N (V (A 5 ""))]])
    , E (V (A 2 "")) (D [C [V (A 1 ""), V (A 3 ""), N (V (A 4 "")), N (V (A 5 ""))]])
    , E (V (A 3 "")) (D [C [T]])
    , E (V (A 4 "")) (D [C [F]])
    ]


compLP3 :: [Form]
compLP3 = weakCompLP3 ++ [ N (V (A 5 "")) ]


weakCompLP4 :: [Form]
weakCompLP4 = [ E (V (A 1 "")) (D [C [V (A 2 ""), N (V (A 1 ""))]]) ]


compLP4 :: [Form]
compLP4 = weakCompLP4 ++ [ N (V (A 2 "")) ]


weakCompLP5 :: [Form]
weakCompLP5 =
    [ E (V (A 1 "")) (D [C [N (V (A 2 ""))]])
    , E (V (A 2 "")) (D [C [V (A 1 "")], C [N (V (A 1 ""))]])
    ]


compLP5 :: [Form]
compLP5 = weakCompLP5
