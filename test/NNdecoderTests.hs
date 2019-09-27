module NNdecoderTests (nnDecoderTests) where

import ExamplesToTest
import Auxiliary
import LogicPrograms
import NNdecoder
import Test.Hspec
import Test.QuickCheck

nnDecoderTests :: IO ()
nnDecoderTests = hspec $ do
    describe "NNdecoder module; decodeNN" $ do
        it "decoding zero clauses" $
            eqLists (decodeNN 0.7 ord1 ioPairs0) rawLP0 `shouldBe` True
        it "decoding one clause" $
            eqLists (decodeNN 0.7 ord1 ioPairs1) rawLP1 `shouldBe` True
        it "decoding three clauses" $
            eqLists (decodeNN 0.7 ord2 ioPairs2) rawLP2 `shouldBe` True


ord0 :: ([Atom], [Atom])
ord0 = ([A 1 "", A 2 ""], [A 1 "", A 2 ""])

ioPairs0 :: [([Int], [Float])]
ioPairs0 = 
    [ ([ 1, 1], [-1.0,-1.0])
    , ([ 1,-1], [-1.0,-1.0])
    , ([-1, 1], [-1.0,-1.0])
    , ([-1,-1], [-1.0,-1.0])
    ]

rawLP0 :: LP
rawLP0 = []


ord1 :: ([Atom], [Atom])
ord1 = ([A 1 "", A 2 ""], [A 1 "", A 2 ""])

ioPairs1 :: [([Int], [Float])]
ioPairs1 = 
    [ ([ 1, 1], [-1.0,-1.0])
    , ([ 1,-1], [-1.0,-1.0])
    , ([-1, 1], [ 1.0,-1.0])
    , ([-1,-1], [-1.0,-1.0])
    ]

rawLP1 :: LP
rawLP1 = [ Cl (A 1 "") [A 2 ""] [A 1 ""] ]


ord2 :: ([Atom], [Atom])
ord2 = ([A 1 "", A 2 "", A 3 ""], [A 1 "", A 2 "", A 3 ""])

ioPairs2 :: [([Int], [Float])]
ioPairs2 = 
    [ ([ 1, 1, 1], [-1.0,-1.0,-1.0])
    , ([ 1, 1,-1], [-1.0,-1.0, 1.0])
    , ([ 1,-1, 1], [-1.0, 1.0,-1.0])
    , ([ 1,-1,-1], [-1.0,-1.0,-1.0])
    , ([-1, 1, 1], [-1.0,-1.0,-1.0])
    , ([-1, 1,-1], [ 1.0,-1.0,-1.0])
    , ([-1,-1, 1], [-1.0,-1.0,-1.0])
    , ([-1,-1,-1], [-1.0,-1.0,-1.0])
    ]

rawLP2 :: LP
rawLP2 =
    [ Cl (A 1 "") [A 2 ""] [A 1 "", A 3 ""]
    , Cl (A 2 "") [A 1 "", A 3 ""] [A 2 ""]
    , Cl (A 3 "") [A 2 "", A 1 ""] [A 3 ""]
    ]
