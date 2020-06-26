module TranslationTpTests (translationTpTests) where

import ExamplesToTest
import Auxiliary
import NeuralNetworks
import TranslationTp
import Test.Hspec
import Test.QuickCheck

translationTpTests :: IO ()
translationTpTests = hspec $ do
    describe "TranslationTp module; baseNN" $ do
        it "for lp4 input layer" $
            (inpLayer $ baseNN lp4 nnFactors) `shouldBe` (inpLayer baseLP4)
            --eqLists (inpLayer $ baseNN lp4 nnFactors) (inpLayer baseLP4) `shouldBe` True


nnFactors :: NNfactors
nnFactors = NNfactors 1.0 1 0.01 0.0 0.1 0.1 


-- Amin = 0.43333333333333335
-- W = 6.2865784775823075
-- bo = 0.0
-- bh = 4.505381242267321
baseLP4 :: NeuralNetwork
baseLP4 = NN
    { inpLayer            = [Neuron "A1" "idem" 0.0 "inp1", Neuron "A2" "idem" 0.0 "inp2", Neuron "inpT" "const" 0.0 "inpT"]
    , hidLayer            = [Neuron "h1" "tanh" 4.505381242267321 "hid1", Neuron "hidT" "tanh" 0.0 "hidT"]
    , outLayer            = [Neuron "A1" "tanh" 0.0 "out1", Neuron "A2" "tanh" 0.0 "out2"]
    , recLayer            = []
    , inpToHidConnections = [Connection "inpT" "hidT" 6.2865784775823075, Connection "inp1" "hid1" (-6.2865784775823075), Connection "inp2" "hid1" 6.2865784775823075]
    , hidToOutConnections = [Connection "hid1" "out1" 6.2865784775823075]
    , recConnections      = []
    }

